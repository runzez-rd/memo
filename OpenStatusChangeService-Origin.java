@Service
public class OpenStatusChangeService {

    @Autowired private OrganizationService organizationService;
    @Autowired private TypeAService typeAService;
    @Autowired private TypeBService typeBService;
    @Autowired private TypeCService typeCService;
    @Autowired private TypeDService typeDService;
    @Autowired private TypeEService typeEService;

    /**
     * 🚩 Hint：通过类型枚举，动态派发到不同的处理函数（每个处理函数绑定了实体结构与具体Service）
     */
    private final Map<TypeEnum, Function<OpenStatusChangeDto, CountResultDto>> openStatusChangeHandlerMap = Map.of(
        TypeEnum.TYPE_A, this::handleTypeA,
        TypeEnum.TYPE_B, this::handleTypeB,
        TypeEnum.TYPE_C, this::handleTypeC,
        TypeEnum.TYPE_D, this::handleTypeD,
        TypeEnum.TYPE_E, this::handleTypeE
    );

    /**
     * 🚩 批量公开处理（设置为 TRUE）
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
     * 🚩 单件非公开处理（设置为 FALSE）
     */
    @Transactional(rollbackFor = Exception.class)
    public CountResultDto cancelOpen(OpenStatusChangeDto condition) {
        condition.setOpenFlag(OpenFlagEnum.FALSE);
        return dispatch(condition);
    }

    /**
     * 🚩 Hint：通过类型枚举选择具体处理方法（责任转交到 handlerMap）
     */
    private CountResultDto dispatch(OpenStatusChangeDto condition) {
        Function<OpenStatusChangeDto, CountResultDto> handler = openStatusChangeHandlerMap.get(condition.getType());
        if (handler == null) {
            throw new UncheckedLogicException("E000", "Unsupported type");
        }
        return handler.apply(condition);
    }

    /**
     * 🚩 Hint：统一的通用处理逻辑，所有子类类型最终都转化为本方法的参数组合
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

    private boolean isPublish(OpenStatusChangeDto condition) {
        return OpenFlagEnum.TRUE.equals(condition.getOpenFlag());
    }

    private String resolveOrgName(Integer orgId) {
        return organizationService.getOne(orgId)
                .orElseThrow(() -> new UncheckedLogicException("E003", "Organization not found"))
                .getName();
    }

    // ===== 以下为各子类型 handler 实现 =====

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

    private CountResultDto handleTypeB(OpenStatusChangeDto condition) {
        return handle(
            condition,
            () -> typeBService.getOne(condition.getId()),
            TypeB::getType,
            TypeB::getOrgId,
            TypeB::getUpdateTime,
            TypeB::getConfirmedStatus,
            TypeB::getOpenFlag,
            TypeB::setOpenFlag,
            TypeB::getId,
            TypeB::getTargetDate,
            TypeB::getOrg,
            typeBService::update,
            isPublish(condition)
        );
    }

    private CountResultDto handleTypeC(OpenStatusChangeDto condition) {
        return handle(
            condition,
            () -> typeCService.getOne(condition.getId()),
            TypeC::getType,
            TypeC::getOrgId,
            TypeC::getUpdateTime,
            TypeC::getConfirmedStatus,
            TypeC::getOpenFlag,
            TypeC::setOpenFlag,
            TypeC::getId,
            TypeC::getTargetDate,
            TypeC::getOrg,
            typeCService::update,
            isPublish(condition)
        );
    }

    private CountResultDto handleTypeD(OpenStatusChangeDto condition) {
        return handle(
            condition,
            () -> typeDService.getOne(condition.getId()),
            TypeD::getType,
            TypeD::getOrgId,
            TypeD::getUpdateTime,
            TypeD::getConfirmedStatus,
            TypeD::getOpenFlag,
            TypeD::setOpenFlag,
            TypeD::getId,
            TypeD::getTargetDate,
            TypeD::getOrg,
            typeDService::update,
            isPublish(condition)
        );
    }

    private CountResultDto handleTypeE(OpenStatusChangeDto condition) {
        return handle(
            condition,
            () -> typeEService.getOne(condition.getId()),
            TypeE::getType,
            TypeE::getOrgId,
            TypeE::getUpdateTime,
            TypeE::getConfirmedStatus,
            TypeE::getOpenFlag,
            TypeE::setOpenFlag,
            TypeE::getId,
            TypeE::getTargetDate,
            TypeE::getOrg,
            typeEService::update,
            isPublish(condition)
        );
    }
}
