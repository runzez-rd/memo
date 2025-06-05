
# Open Status Change 公共处理逻辑设计文档(gpt依据笔记修改后版本)

## 1. 背景说明

本模块处理公开状态的更新操作，主要包括两类：

* **批量公开操作**（`applyOpen`）：将多条数据设为“公开”。
* **取消公开操作**（`cancelOpen`）：将单条数据设为“未公开”。

由于业务中涉及多个实体（如 TYPE\_A \~ TYPE\_E），每种类型对应不同数据表和服务类，但数据结构及更新逻辑相似，因此需要提取共通处理逻辑，实现代码复用与易扩展性。

---

## 2. 架构设计

### 2.1 请求与响应结构

* **请求参数类：** `OpenStatusChangeDto`
* **响应结果类：** `CountResultDto`
* 使用 `TypeEnum` 字段区分不同业务实体类型。

---

### 2.2 处理器映射机制

通过 `Map<TypeEnum, Function<OpenStatusChangeDto, CountResultDto>>` 构建分发映射，实现类型与业务处理逻辑解耦：

```java
private final Map<TypeEnum, Function<OpenStatusChangeDto, CountResultDto>> handlerMap = Map.of(
    TYPE_A, this::handleTypeA,
    TYPE_B, this::handleTypeB,
    TYPE_C, this::handleTypeC,
    TYPE_D, this::handleTypeD,
    TYPE_E, this::handleTypeE
);
```

统一通过 `dispatch()` 方法进行类型选择与逻辑调用：

```java
private CountResultDto dispatch(OpenStatusChangeDto condition) {
    Function<OpenStatusChangeDto, CountResultDto> handler = handlerMap.get(condition.getType());
    if (handler == null) {
        throw new UncheckedLogicException("E000", "Unsupported type");
    }
    return handler.apply(condition);
}
```

---

### 2.3 业务方法入口

```java
@Transactional
public List<CountResultDto> applyOpen(List<OpenStatusChangeDto> list) {
    return list.stream()
               .peek(dto -> dto.setOpenFlag(OpenFlagEnum.TRUE))
               .map(this::dispatch)
               .toList();
}

@Transactional
public CountResultDto cancelOpen(OpenStatusChangeDto dto) {
    dto.setOpenFlag(OpenFlagEnum.FALSE);
    return dispatch(dto);
}
```

---

## 3. 共通处理方法：`handle`

所有公开/取消公开处理流程具备相同步骤，仅实体行为不同。将其抽象为一个泛型方法 `handle<T>`，通过 Lambda 注入不同实体的操作函数。

```java
private <T> CountResultDto handle(
    OpenStatusChangeDto condition,
    Supplier<Optional<T>> entityFetcher,
    Function<T, TypeEnum> typeGetter,
    Function<T, Integer> orgIdGetter,
    Function<T, LocalDateTime> updateTimeGetter,
    Function<T, ConfirmedStatusEnum> confirmedGetter,
    Function<T, OpenFlagEnum> openFlagGetter,
    BiConsumer<T, OpenFlagEnum> openFlagSetter,
    Function<T, Integer> idGetter,
    Function<T, LocalDate> targetDateGetter,
    Function<T, Organization> orgModelGetter,
    Function<T, T> updater,
    boolean publish
) {
    T entity = entityFetcher.get().orElse(null);

    boolean notFound = entity == null;
    boolean invalidType = !notFound && !condition.getType().equals(typeGetter.apply(entity));
    boolean conflict = !notFound && !Objects.equals(condition.getUpdateTime(), updateTimeGetter.apply(entity));
    boolean orgMismatch = !notFound && !condition.getOrgId().equals(orgIdGetter.apply(entity));
    if (notFound || invalidType || conflict || orgMismatch) {
        throw new UncheckedLogicException("E001", "Invalid input");
    }

    boolean isDraft = ConfirmedStatusEnum.DRAFT.equals(confirmedGetter.apply(entity));
    boolean isAlreadyOpen = OpenFlagEnum.TRUE.equals(openFlagGetter.apply(entity));
    boolean isAlreadyClosed = OpenFlagEnum.FALSE.equals(openFlagGetter.apply(entity));
    if (isDraft || (publish && isAlreadyOpen) || (!publish && isAlreadyClosed)) {
        throw new UncheckedLogicException("E002", publish ? "Already open" : "Already closed");
    }

    openFlagSetter.accept(entity, publish ? OpenFlagEnum.TRUE : OpenFlagEnum.FALSE);
    T updated = updater.apply(entity);

    CountResultDto dto = new CountResultDto();
    dto.setType(typeGetter.apply(updated));
    dto.setId(idGetter.apply(updated));
    dto.setTargetDate(targetDateGetter.apply(updated));
    dto.setUpdateTime(updateTimeGetter.apply(updated));
    dto.setOrgId(orgIdGetter.apply(updated));
    dto.setOrgName(resolveOrgName(orgIdGetter.apply(updated)));
    dto.setConfirmedStatus(confirmedGetter.apply(updated));
    dto.setOpenFlag(openFlagGetter.apply(updated));
    return dto;
}
```

---

### 3.1 示例：TypeA 处理逻辑

```java
private CountResultDto handleTypeA(OpenStatusChangeDto condition) {
    return handle(
        condition,
        () -> typeAService.getOne(condition.getId()),
        TypeA::getType,
        TypeA::getOrgId,
        TypeA::getUpdateTime,
        TypeA::getConfirmedStatus,
        TypeA::getOpenFlag,
        TypeA::setOpenFlag,
        TypeA::getId,
        TypeA::getTargetDate,
        TypeA::getOrg,
        typeAService::update,
        isPublish(condition)
    );
}
```

---

## 4. 存在的问题与优化方向

| 问题                   | 说明                                         | 建议                                        |
| -------------------- | ------------------------------------------ | ----------------------------------------- |
| **handlerMap 依赖硬编码** | 每次扩展都需修改代码                                 | 考虑使用 SPI 或配置驱动的策略注册方式                     |
| **`handle` 方法参数繁杂**  | 可读性差、调用复杂                                  | 引入封装类 `EntityHandler<T>` 管理所有 Lambda      |
| **lambda 存在重复**      | 每种类型的代码几乎一致                                | 使用模版方法设计或接口标准化实体操作                        |
| **状态控制与入口耦合**        | `applyOpen/cancelOpen` 外部设置 `OpenFlag` 不直观 | 改为 `dispatch(condition, publish)` 提高职责清晰度 |
| **无法热更新或配置驱动**       | 所有注册逻辑均在编译时固定                              | 支持配置文件或数据库驱动的动态注册                         |

---

## 5. 长期演化建议

为了系统长期扩展性，建议引入：

* **注册中心机制**：管理所有处理器的注册与查询，支持热插拔。
* **配置驱动**：使用 YAML/DB 存储各类型映射信息，降低代码侵入。
* **策略模式或责任链**：适用于处理流程可能变动或层次扩展的业务场景。

---

## 6. 总结

该设计在保持逻辑一致性的同时，通过函数参数化与枚举分发机制，降低了重复代码。适用于当前场景下中等规模的状态变更业务。

但随着系统复杂度提升，需要逐步引入：

* 动态扩展机制（注册中心）
* 更强的接口隔离（职责分层）
* 更低耦合的配置方式（配置+反射+策略）

以提升系统的**弹性、维护性和演化能力**。