/*
 * 进一步优化说明（与第一版memo.java对比）：
 * 1. 用 HandlerMeta 元数据对象封装所有 getter/setter，registerHandler 参数更少，结构更清晰。
 * 2. registerHandler 直接传 service::getOne，避免重复 lambda。
 * 3. 注册逻辑更简洁，后续类型扩展更方便。
 * 4. 代码更易维护和阅读，减少硬编码和出错概率。
 * 5. 便于后续自动注册和批量注册。
 *
 * 优化思维成长总结：
 * - 从“能用”到“优雅”：减少复制粘贴，关注共性，统一注册和分发。
 * - 关注“变化”与“不变”：把变化抽象成参数或元数据，把不变固化为统一流程。
 * - 面向扩展和维护：新增类型只需维护一行注册代码，极大降低维护成本和出错概率。
 * - 泛型与元数据思想：用泛型和元数据对象，提升类型安全和自动化能力。
 */

@Service
public class OpenStatusChangeService {

    @Autowired private OrganizationService organizationService;
    @Autowired private TypeAService typeAService;
    @Autowired private TypeBService typeBService;
    @Autowired private TypeCService typeCService;
    @Autowired private TypeDService typeDService;
    @Autowired private TypeEService typeEService;

    // handlerMap 依然为可变 HashMap，类型分发核心
    private final Map<TypeEnum, Function<OpenStatusChangeDto, CountResultDto>> openStatusChangeHandlerMap = new HashMap<>();

    // HandlerMeta 封装所有 getter/setter，便于注册和维护
    public static class HandlerMeta<T> {
        public final Function<Integer, Optional<T>> entityFetcher;
        public final Function<T, TypeEnum> typeGetter;
        public final Function<T, Integer> orgIdGetter;
        public final Function<T, LocalDateTime> updateTimeGetter;
        public final Function<T, ConfirmedStatusEnum> confirmedGetter;
        public final Function<T, OpenFlagEnum> openFlagGetter;
        public final BiConsumer<T, OpenFlagEnum> openFlagSetter;
        public final Function<T, Integer> idGetter;
        public final Function<T, LocalDate> targetDateGetter;
        public final Function<T, Organization> orgModelGetter;
        public final Function<T, T> updater;

        public HandlerMeta(
            Function<Integer, Optional<T>> entityFetcher,
            Function<T, TypeEnum> typeGetter,
            Function<T, Integer> orgIdGetter,
            Function<T, LocalDateTime> updateTimeGetter,
            Function<T, ConfirmedStatusEnum> confirmedGetter,
            Function<T, OpenFlagEnum> openFlagGetter,
            BiConsumer<T, OpenFlagEnum> openFlagSetter,
            Function<T, Integer> idGetter,
            Function<T, LocalDate> targetDateGetter,
            Function<T, Organization> orgModelGetter,
            Function<T, T> updater
        ) {
            this.entityFetcher = entityFetcher;
            this.typeGetter = typeGetter;
            this.orgIdGetter = orgIdGetter;
            this.updateTimeGetter = updateTimeGetter;
            this.confirmedGetter = confirmedGetter;
            this.openFlagGetter = openFlagGetter;
            this.openFlagSetter = openFlagSetter;
            this.idGetter = idGetter;
            this.targetDateGetter = targetDateGetter;
            this.orgModelGetter = orgModelGetter;
            this.updater = updater;
        }
    }

    // 注册所有类型的 handler，参数更少更清晰
    @PostConstruct
    private void initHandlerMap() {
        registerHandler(TypeEnum.TYPE_A, new HandlerMeta<>(
            typeAService::getOne, TypeA::getType, TypeA::getOrgId, TypeA::getUpdateTime, TypeA::getConfirmedStatus,
            TypeA::getOpenFlag, TypeA::setOpenFlag, TypeA::getId, TypeA::getTargetDate, TypeA::getOrg, typeAService::update
        ));
        registerHandler(TypeEnum.TYPE_B, new HandlerMeta<>(
            typeBService::getOne, TypeB::getType, TypeB::getOrgId, TypeB::getUpdateTime, TypeB::getConfirmedStatus,
            TypeB::getOpenFlag, TypeB::setOpenFlag, TypeB::getId, TypeB::getTargetDate, TypeB::getOrg, typeBService::update
        ));
        registerHandler(TypeEnum.TYPE_C, new HandlerMeta<>(
            typeCService::getOne, TypeC::getType, TypeC::getOrgId, TypeC::getUpdateTime, TypeC::getConfirmedStatus,
            TypeC::getOpenFlag, TypeC::setOpenFlag, TypeC::getId, TypeC::getTargetDate, TypeC::getOrg, typeCService::update
        ));
        registerHandler(TypeEnum.TYPE_D, new HandlerMeta<>(
            typeDService::getOne, TypeD::getType, TypeD::getOrgId, TypeD::getUpdateTime, TypeD::getConfirmedStatus,
            TypeD::getOpenFlag, TypeD::setOpenFlag, TypeD::getId, TypeD::getTargetDate, TypeD::getOrg, typeDService::update
        ));
        registerHandler(TypeEnum.TYPE_E, new HandlerMeta<>(
            typeEService::getOne, TypeE::getType, TypeE::getOrgId, TypeE::getUpdateTime, TypeE::getConfirmedStatus,
            TypeE::getOpenFlag, TypeE::setOpenFlag, TypeE::getId, TypeE::getTargetDate, TypeE::getOrg, typeEService::update
        ));
    }

    // registerHandler 只需传 meta，极简注册
    private <T> void registerHandler(TypeEnum type, HandlerMeta<T> meta) {
        openStatusChangeHandlerMap.put(type, condition -> handle(
            condition,
            () -> meta.entityFetcher.apply(condition.getId()),
            meta.typeGetter,
            meta.orgIdGetter,
            meta.updateTimeGetter,
            meta.confirmedGetter,
            meta.openFlagGetter,
            meta.openFlagSetter,
            meta.idGetter,
            meta.targetDateGetter,
            meta.orgModelGetter,
            meta.updater,
            isPublish(condition)
        ));
    }

    /**
     * 批量公开处理（设置为 TRUE）
     */
    @Transactional(rollbackFor = Exception.class)
    public List<CountResultDto> applyOpen(List<OpenStatusChangeDto> conditionList) {
        return conditionList.stream()
            .map(c -> {
                c.setOpenFlag(OpenFlagEnum.TRUE);
                return dispatch(c);
            })
            .toList();
    }

    /**
     * 单件非公开处理（设置为 FALSE）
     */
    @Transactional(rollbackFor = Exception.class)
    public CountResultDto cancelOpen(OpenStatusChangeDto condition) {
        condition.setOpenFlag(OpenFlagEnum.FALSE);
        return dispatch(condition);
    }

    /**
     * 通过类型枚举选择具体处理方法（责任转交到 handlerMap）
     */
    private CountResultDto dispatch(OpenStatusChangeDto condition) {
        Function<OpenStatusChangeDto, CountResultDto> handler = openStatusChangeHandlerMap.get(condition.getType());
        if (handler == null) {
            throw new UncheckedLogicException("E000", "Unsupported type");
        }
        return handler.apply(condition);
    }

    /**
     * 统一的通用处理逻辑，所有子类类型最终都转化为本方法的参数组合
     */
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

    // 判断是否为公开操作
    private boolean isPublish(OpenStatusChangeDto condition) {
        return OpenFlagEnum.TRUE.equals(condition.getOpenFlag());
    }

    // 组织名称解析
    private String resolveOrgName(Integer orgId) {
        return organizationService.getOne(orgId)
                .orElseThrow(() -> new UncheckedLogicException("E003", "Organization not found"))
                .getName();
    }
}