# memo
## OpenStatusChangeService
- base逻辑：OpenStatusChangeService-Origin.java
- Copilot优化后逻辑：OpenStatusChangeService-Copilot.java
### base逻辑
业务流程有两种，都是更新系业务：
- 将多条业务数据的状态从未公开设定成公开，applyOpen
- 将一条业务数据的状态从公开还原成未公开，cancelOpen

该业务的特殊的部分在于，根据TypeEnum的种类不同，更新的实体对象不同。但是另一方面，更新操作的表都是单表，并且这些表的结构类似。（实际上从表结构和业务的设计上优化更有意义）<br/>
<br/>
对此为了尽可能的复用代码，尽可能的抽出共通逻辑：<br/>
<br/>
因为表结构的类似，更新参数的OpenStatusChangeDto，和更新后返回的展示结构CountResultDto可以复用。通过对象属性中的TypeEnum做不同实体的区分。<br/>
<br/>
不同的举对应不同的业务方法。考虑到此时的枚举数量已经多达5个，后续有有进一步扩展的可能。所以比起使用大量的增强switch或大量的if判断，想将通过枚举选择具体业务方法的处理抽离出来。<br/>
<br/>
枚举和业务方法，以枚举和lambda的key-value的形式，注册成一个openStatusChangeHandlerMap。业务方法的具体执行，只需要通过枚举取得lambda执行，不需要考虑判断逻辑（对应方法：dispatch）。<br/>

```java

    // 注册map，每个枚举类型绑定一个实际业务方法
    private final Map<TypeEnum, Function<OpenStatusChangeDto, CountResultDto>> openStatusChangeHandlerMap = Map.of(
        TypeEnum.TYPE_A, this::handleTypeA,
        TypeEnum.TYPE_B, this::handleTypeB,
        TypeEnum.TYPE_C, this::handleTypeC,
        TypeEnum.TYPE_D, this::handleTypeD,
        TypeEnum.TYPE_E, this::handleTypeE
    );

    // 通过类型枚动态举选择具体处理方法
    private CountResultDto dispatch(OpenStatusChangeDto condition) {
        Function<OpenStatusChangeDto, CountResultDto> handler = openStatusChangeHandlerMap.get(condition.getType());
        if (handler == null) {
            throw new UncheckedLogicException("E000", "Unsupported type");
        }
        return handler.apply(condition);
    }
    
    // 批量公开业务，循环调用单次的公开业务
    @Transactional(rollbackFor = Exception.class)
    public List<CountResultDto> applyOpen(List<OpenStatusChangeDto> conditionList) {
        return conditionList.stream()
            .map(c -> {
                c.setOpenFlag(OpenFlagEnum.TRUE);
                return dispatch(c);
            })
            .toList();
    }

    // 单次未更新业务，调用一次未公开业务
    @Transactional(rollbackFor = Exception.class)
    public CountResultDto cancelOpen(OpenStatusChangeDto condition) {
        condition.setOpenFlag(OpenFlagEnum.FALSE);
        return dispatch(condition);
    }
```

<br/>
然后思考具体业务方法的复用：<br/>
<br/>
如果为每个实体的公开和未公开业务各写一个业务方法，就是10个方法。重复逻辑过多，也不好维护。<br/>
<br/>
同一张表的公开和未公开业务之间，有大量通用部分。大体逻辑上都是，检索db数据，做业务校验，通过校验后更新db，更新后把新的实体映射成前端需要的格式返回。<br/>
<br/>
公开和未公开的业务的不同集中在业务校验，和状态值更新的部分，所以保留共通的部分，不同的部分通过lambda作为参数传进来。这样公开和未公开业务抽离成了更新处理业务的共通方法。方法由10个缩减成5个。<br/>
<br/>
5个不同表之间的更新业务，也有着大量的重复逻辑。从行为上看，这写方法都是同一套业务流程，差别仅在于不同实体之间的具体操作。所以结合泛型和lambda把实体操作的行为抽象化，以参数的形式传进共通方法。于是抽象出方法handle。<br/>

```java

    // 更新业务通用方法  
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
        // 针对不同实体的更新前检索，返回泛型
        T entity = entityFetcher.get().orElse(null);

        boolean notFound = entity == null;
        boolean invalidType = !notFound && !condition.getType().equals(typeGetter.apply(entity));
        boolean conflict = !notFound && !Objects.equals(condition.getUpdateTime(), updateTimeGetter.apply(entity));
        boolean orgMismatch = !notFound && !condition.getOrgId().equals(orgIdGetter.apply(entity));
        // 业务校验：更新条件参数校验
        if (notFound || invalidType || conflict || orgMismatch) {
            throw new UncheckedLogicException("E001", "Invalid input");
        }

        boolean isDraft = ConfirmedStatusEnum.DRAFT.equals(confirmedGetter.apply(entity));
        boolean isAlreadyOpen = OpenFlagEnum.TRUE.equals(openFlagGetter.apply(entity));
        boolean isAlreadyClosed = OpenFlagEnum.FALSE.equals(openFlagGetter.apply(entity));
        // 业务校验：更新前状态变更状态确认
        if (isDraft || (publish && isAlreadyOpen) || (!publish && isAlreadyClosed)) {
            throw new UncheckedLogicException("E002", publish ? "Already open" : "Already closed");
        }

        // 判断是状态变更为公开或是未公开
        openFlagSetter.accept(entity, publish ? OpenFlagEnum.TRUE : OpenFlagEnum.FALSE);
        // 针对不同实体的具体更新处理，返回更新后db的完整实体
        T updated = updater.apply(entity);

        // 不同实体统一封装成相同的数据结构供前端展示
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

<br/>
具体的方法handleTypeA等处理，只需要把不同实体的操作行为作为实际参数传进去即可。参考handleTypeA ~ handleTypeE。<br/>

```java

    // 具体的业务方法
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

<br/>
反思：<br/>

- 虽然做了map的提取，实际上仍旧是hard code，扩展仍旧不方便，或许需要在db或是配置文件中设置类型，否则任何修改都需要重新部署。
- 共通方法handle参数过多，提取共通的结果牺牲了可读性。而且lambda存在大量重复。
- 两个业务方法之间存在强耦合，比起减少重复代码，是不是api之间解耦更加重要。
