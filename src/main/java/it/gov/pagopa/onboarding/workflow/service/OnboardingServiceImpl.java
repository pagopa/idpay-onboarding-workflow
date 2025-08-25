package it.gov.pagopa.onboarding.workflow.service;

import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.decrypt.DecryptRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.*;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.enums.AutomatedCriteria;
import it.gov.pagopa.onboarding.workflow.event.producer.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.event.producer.OutcomeProducer;
import it.gov.pagopa.onboarding.workflow.exception.custom.*;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import it.gov.pagopa.onboarding.workflow.repository.SelfDeclarationRepository;
import it.gov.pagopa.onboarding.workflow.service.common.OnboardingServiceCommonImpl;
import it.gov.pagopa.onboarding.workflow.utils.AuditUtilities;
import it.gov.pagopa.onboarding.workflow.utils.Utilities;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.support.PageableExecutionUtils;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.READMISSION_NOT_ALLOWED;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.SUSPENSION_NOT_ALLOWED;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.*;

@Slf4j
@Service
public class OnboardingServiceImpl extends OnboardingServiceCommonImpl implements OnboardingService {

  public static final String PUT_TC_CONSENT = "PUT_TC_CONSENT";
  public static final String SUSPENSION = "SUSPENSION";
  public static final String READMISSION = "READMISSION";
  public static final String GET_ONBOARDING_FAMILY = "GET_ONBOARDING_FAMILY";
  public static final String EMPTY = "";
  public static final String COMMA_DELIMITER = ",";

  private final int pageSize;
  private final long delayTime;
  private final OutcomeProducer outcomeProducer;
  private final DecryptRestConnector decryptRestConnector;


  public OnboardingServiceImpl(@Value("${app.delete.paginationSize}") int pageSize,
                               @Value("${app.delete.delayTime}") long delayTime,
                               SelfDeclarationRepository selfDeclarationRepository,
                               ConsentMapper consentMapper,
                               OnboardingProducer onboardingProducer,
                               OutcomeProducer outcomeProducer,
                               InitiativeRestConnector initiativeRestConnector,
                               DecryptRestConnector decryptRestConnector,
                               AdmissibilityRestConnector admissibilityRestConnector,
                               AuditUtilities auditUtilities,
                               Utilities utilities,
                               OnboardingRepository onboardingRepository
                               ){
    super(auditUtilities, utilities, onboardingRepository, admissibilityRestConnector, selfDeclarationRepository, consentMapper, onboardingProducer, initiativeRestConnector);
    this.pageSize = pageSize;
    this.delayTime = delayTime;
    this.outcomeProducer = outcomeProducer;
    this.decryptRestConnector = decryptRestConnector;
  }

  @Override
  public void putTcConsent(String initiativeId, String userId) {
    long startTime = System.currentTimeMillis();

    InitiativeDTO initiativeDTO = getInitiative(initiativeId);

    Onboarding onboarding = onboardingRepository.findById(Onboarding.buildId(initiativeId, userId))
        .orElse(null);

    if (onboarding != null && !List.of(OnboardingWorkflowConstants.INVITED,OnboardingWorkflowConstants.DEMANDED).contains(onboarding.getStatus())) {
      checkStatus(onboarding);

      log.info("[PUT_TC_CONSENT] User has already accepted T&C");
      auditUtilities.logTCIdemp(userId, initiativeId, onboarding.getChannel());
      performanceLog(startTime, PUT_TC_CONSENT, userId, initiativeId);
      return;
    }

    LocalDateTime localDateTime = LocalDateTime.now();

    if (onboarding == null) {
      onboarding = new Onboarding(initiativeId, userId);
      onboarding.setCreationDate(localDateTime);
      onboarding.setUpdateDate(localDateTime);
    }

    if (!(Boolean.TRUE.equals(initiativeDTO.getGeneral().getRankingEnabled())
            && OnboardingWorkflowConstants.DEMANDED.equals(onboarding.getStatus()))){
      checkDates(initiativeDTO, onboarding);
    }

    if (!OnboardingWorkflowConstants.DEMANDED.equals(onboarding.getStatus())){
      checkBudget(initiativeDTO, onboarding);
    }

    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);
    onboarding.setTcAcceptTimestamp(localDateTime);
    onboarding.setUpdateDate(localDateTime);
    onboardingRepository.save(onboarding);
    auditUtilities.logTC(userId, initiativeId, onboarding.getChannel());
    performanceLog(startTime, PUT_TC_CONSENT, userId, initiativeId);
  }

  private void setStatus(Onboarding onboarding, String status, LocalDateTime date, String rejectionReasons) {
    if (status.equals(OnboardingWorkflowConstants.JOINED)) {
      status = OnboardingWorkflowConstants.ONBOARDING_OK;
    }
    if (status.equals(OnboardingWorkflowConstants.REJECTED)) {
      status = OnboardingWorkflowConstants.ONBOARDING_KO;
    }

    onboarding.setStatus(status);
    if (status.equals(OnboardingWorkflowConstants.ONBOARDING_OK)) {
      onboarding.setOnboardingOkDate(date);
      auditUtilities.logOnboardingComplete(onboarding.getUserId(), onboarding.getInitiativeId(), onboarding.getChannel(), date);
    }
    if (status.equals(OnboardingWorkflowConstants.ON_EVALUATION)) {
      onboarding.setCriteriaConsensusTimestamp(date);
      auditUtilities.logOnboardingOnEvaluation(onboarding.getUserId(), onboarding.getInitiativeId(), onboarding.getChannel(), date);
    }
    if (status.equals(OnboardingWorkflowConstants.ONBOARDING_KO)) {
      onboarding.setOnboardingKODate(date);
      onboarding.setDetailKO(rejectionReasons);
      auditUtilities.logOnboardingKOWithReason(onboarding.getUserId(), onboarding.getInitiativeId(), onboarding.getChannel(), rejectionReasons);
      if (rejectionReasons.contains(OnboardingWorkflowConstants.OUT_OF_RANKING)) {
        onboarding.setStatus(OnboardingWorkflowConstants.ELIGIBLE_KO);
      }
    }
    onboarding.setUpdateDate(LocalDateTime.now());
    onboardingRepository.save(onboarding);
    auditUtilities.logOnboardingComplete(onboarding.getUserId(), onboarding.getInitiativeId(),
        onboarding.getChannel(), date);
  }

  @Override
  public RequiredCriteriaDTO checkPrerequisites(String initiativeId, String userId,
      String channel) {
    long startTime = System.currentTimeMillis();

    InitiativeDTO initiativeDTO = getInitiative(initiativeId);
    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);

    checkStatus(onboarding);
    if (onboarding.getStatus().equals(OnboardingWorkflowConstants.ACCEPTED_TC)) {
      if (onboarding.getDemandedDate() == null){
        checkDates(initiativeDTO, onboarding);
        checkBudget(initiativeDTO, onboarding);
      }
      if (onboarding.getDemandedDate() != null){
        checkDates(initiativeDTO, onboarding);
      }
      checkFamilyUnit(onboarding, initiativeDTO);
    }

    onboarding.setChannel(channel);
    RequiredCriteriaDTO dto = null;

    if (!checkWhitelist(onboarding, initiativeDTO)) {
      dto = getCriteriaLists(initiativeDTO);
    }
    onboardingRepository.save(onboarding);
    auditUtilities.logPDND(userId, initiativeId, onboarding.getChannel());
    performanceLog(startTime, "CHECK_PREREQUISITES", userId, initiativeId);
    return dto;
  }

  private void checkFamilyUnit(Onboarding onboarding, InitiativeDTO initiativeDTO) {
    if (OnboardingWorkflowConstants.BENEFICIARY_TYPE_NF.equals(initiativeDTO.getGeneral().getBeneficiaryType()) &&
            onboarding.getDemandedDate() != null) {
      setStatus(onboarding, OnboardingWorkflowConstants.ON_EVALUATION, LocalDateTime.now(), null);
      outcomeProducer.sendOutcome(createEvaluationDto(onboarding, initiativeDTO, OnboardingWorkflowConstants.JOINED));
    }
  }

  private boolean checkWhitelist(Onboarding onboarding, InitiativeDTO initiativeDTO) {
    if (Boolean.FALSE.equals(initiativeDTO.getGeneral().getBeneficiaryKnown())) {
      return false;
    }
    if (OnboardingWorkflowConstants.STATUS_IDEMPOTENT.contains(onboarding.getStatus())) {
      return true;
    }
    if (onboarding.getInvitationDate() == null) {
      setStatus(onboarding, OnboardingWorkflowConstants.ONBOARDING_KO, LocalDateTime.now(),
              OnboardingWorkflowConstants.ERROR_WHITELIST);
      auditUtilities.logOnboardingKOWhiteList(onboarding.getUserId(), onboarding.getInitiativeId(), onboarding.getChannel(), LocalDateTime.now());
      throw new UserNotInWhitelistException(String.format(ERROR_WHITELIST_MSG, initiativeDTO.getInitiativeId()));
    }
    setStatus(onboarding, OnboardingWorkflowConstants.ON_EVALUATION, LocalDateTime.now(), null);
    outcomeProducer.sendOutcome(createEvaluationDto(onboarding, initiativeDTO, OnboardingWorkflowConstants.ONBOARDING_OK));
    return true;
  }

  private EvaluationDTO createEvaluationDto(Onboarding onboarding,
      InitiativeDTO initiativeDTO, String status) {
    EvaluationDTO dto = new EvaluationDTO();
    dto.setInitiativeId(onboarding.getInitiativeId());
    dto.setInitiativeName(initiativeDTO.getInitiativeName());
    dto.setInitiativeEndDate(initiativeDTO.getGeneral().getEndDate());
    dto.setUserId(onboarding.getUserId());
    dto.setFamilyId(onboarding.getFamilyId());
    dto.setOrganizationId(initiativeDTO.getOrganizationId());
    dto.setAdmissibilityCheckDate(LocalDateTime.now());
    dto.setStatus(status);
    dto.setOnboardingRejectionReasons(List.of());
    dto.setBeneficiaryBudgetCents(null != initiativeDTO.getGeneral().getBeneficiaryBudget() ? initiativeDTO.getGeneral().getBeneficiaryBudget().multiply(BigDecimal.valueOf(100)).longValue() : null);
    dto.setInitiativeRewardType(initiativeDTO.getInitiativeRewardType());
    dto.setOrganizationName(initiativeDTO.getOrganizationName());
    dto.setIsLogoPresent(initiativeDTO.getIsLogoPresent());
    dto.setServiceId(null != initiativeDTO.getAdditionalInfo() ? initiativeDTO.getAdditionalInfo().getServiceId() : null);
    return dto;
  }

  private RequiredCriteriaDTO getCriteriaLists(InitiativeDTO initiativeDTO) {

    RequiredCriteriaDTO requiredCriteriaDTO = new RequiredCriteriaDTO();
    List<PDNDCriteriaDTO> pdndCriteria = new ArrayList<>();

    initiativeDTO.getBeneficiaryRule().getAutomatedCriteria().forEach(item ->
        pdndCriteria.add(new PDNDCriteriaDTO(item.getCode(),
            AutomatedCriteria.valueOf(item.getCode()).getDescription(), item.getAuthority(),
            item.getValue(), item.getOperator(), item.getValue2()))
    );

    requiredCriteriaDTO.setSelfDeclarationList(
        initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria());
    requiredCriteriaDTO.setPdndCriteria(pdndCriteria);
    auditUtilities.logGetListPDND(initiativeDTO.getInitiativeId());
    return requiredCriteriaDTO;
  }

  @Override
  public OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId) {
    long startTime = System.currentTimeMillis();
    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);
    performanceLog(startTime, "GET_ONBOARDING_STATUS", userId, initiativeId);
    log.info("[ONBOARDING_STATUS] Onboarding status is: {}", onboarding.getStatus());
    return new OnboardingStatusDTO(onboarding.getStatus(), onboarding.getUpdateDate(), onboarding.getOnboardingOkDate() != null ? onboarding.getOnboardingOkDate() : null);
  }

  @Override
  public ResponseInitiativeOnboardingDTO getOnboardingStatusList(String initiativeId,
      String userId, LocalDateTime startDate, LocalDateTime endDate, String status,
      Pageable pageable) {
    long startTime = System.currentTimeMillis();

    if (pageable != null && pageable.getPageSize() > 15 ){
      throw new PageSizeNotAllowedException(ERROR_MAX_NUMBER_FOR_PAGE_MSG);
    }
    List<OnboardingStatusCitizenDTO> onboardingStatusCitizenDTOS = new ArrayList<>();
    Criteria criteria = onboardingRepository.getCriteria(initiativeId, userId, status, startDate,
        endDate);
    List<Onboarding> onboardinglist = onboardingRepository.findByFilter(criteria, pageable);
    long count = onboardingRepository.getCount(criteria);
    final Page<Onboarding> result = PageableExecutionUtils.getPage(onboardinglist,
        this.getPageable(pageable), () -> count);
    for (Onboarding o : onboardinglist) {
      OnboardingStatusCitizenDTO onboardingStatusCitizenDTO = new OnboardingStatusCitizenDTO(
              o.getUserId(), o.getStatus(),
              o.getUpdateDate() != null ? o.getUpdateDate().toString() : EMPTY,
              o.getFamilyId());
      onboardingStatusCitizenDTOS.add(onboardingStatusCitizenDTO);
    }
    performanceLog(startTime, "GET_ONBOARDING_STATUS_LIST", userId, initiativeId);
    return new ResponseInitiativeOnboardingDTO(onboardingStatusCitizenDTOS, result.getNumber(),
        result.getSize(), (int) result.getTotalElements(), result.getTotalPages());
  }

  @Override
  public void saveConsent(ConsentPutDTO consentPutDTO, String userId) {
    long startTime = System.currentTimeMillis();

    Onboarding onboarding = findByInitiativeIdAndUserId(consentPutDTO.getInitiativeId(), userId);

    if (OnboardingWorkflowConstants.STATUS_IDEMPOTENT.contains(onboarding.getStatus())) {
      return;
    }
    checkStatus(onboarding);

    InitiativeDTO initiativeDTO = getInitiative(consentPutDTO.getInitiativeId());

    checkDates(initiativeDTO, onboarding);
    checkBudget(initiativeDTO, onboarding);

    if (!initiativeDTO.getBeneficiaryRule().getAutomatedCriteria().isEmpty()
        && !consentPutDTO.isPdndAccept()) {
      performanceLog(startTime, "SAVE_CONSENT", userId, initiativeDTO.getInitiativeId());
      auditUtilities.logOnboardingKOWithReason(userId, initiativeDTO.getInitiativeId(), onboarding.getChannel(),
              OnboardingWorkflowConstants.ERROR_PDND_AUDIT);
      onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
      onboardingRepository.save(onboarding);
      throw new PDNDConsentDeniedException(String.format(ERROR_PDND_MSG, consentPutDTO.getInitiativeId()));
    }

    //selfDeclaration(initiativeDTO, consentPutDTO, userId);
    onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
    onboarding.setPdndAccept(consentPutDTO.isPdndAccept());
    LocalDateTime localDateTime = LocalDateTime.now();
    onboarding.setCriteriaConsensusTimestamp(localDateTime);
    onboarding.setUpdateDate(localDateTime);
    OnboardingDTO onboardingDTO = consentMapper.map(onboarding);
    // ServiceID Setter
    onboardingDTO.setServiceId(initiativeDTO.getAdditionalInfo().getServiceId());
    onboardingProducer.sendSaveConsent(onboardingDTO);
    onboardingRepository.save(onboarding);
    performanceLog(startTime, "SAVE_CONSENT", userId, initiativeDTO.getInitiativeId());
  }

  @Override
  public void deactivateOnboarding(String initiativeId, String userId, String deactivationDate) {
    long startTime = System.currentTimeMillis();

    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);

    onboarding.setStatus(OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED);
    onboarding.setRequestDeactivationDate(LocalDateTime.parse(deactivationDate));
    onboarding.setUpdateDate(LocalDateTime.parse(deactivationDate));
    onboardingRepository.save(onboarding);
    log.info("[DEACTIVATE_ONBOARDING] Onboarding disabled, date: {}", deactivationDate);
    auditUtilities.logDeactivate(userId, initiativeId, onboarding.getChannel(), LocalDateTime.parse(deactivationDate));
    performanceLog(startTime, "DEACTIVATE_ONBOARDING", userId, initiativeId);
  }

  @Override
  public void rollback(String initiativeId, String userId) {
    Onboarding onboarding = onboardingRepository.findById(Onboarding.buildId(initiativeId, userId))
        .orElse(null);
    if (onboarding != null && onboarding.getStatus()
        .equals(OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED)) {
      log.info("[ROLLBACK] Onboarding before rollback: {}", onboarding);
      onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
      onboarding.setRequestDeactivationDate(null);
      onboarding.setUpdateDate(onboarding.getOnboardingOkDate());
      onboardingRepository.save(onboarding);
      log.info("[ROLLBACK] Onboarding after rollback: {}", onboarding);
      auditUtilities.logRollback(userId, initiativeId, onboarding.getChannel());
    }

  }

  @Override
  public void completeOnboarding(EvaluationDTO evaluationDTO) {
    long startTime = System.currentTimeMillis();

    Onboarding onboarding = onboardingRepository.findById(Onboarding.buildId(evaluationDTO.getInitiativeId(), evaluationDTO.getUserId()))
            .orElse(null);

    Set<String> onboardingRejectionReasonsCode = Optional.ofNullable(evaluationDTO.getOnboardingRejectionReasons())
            .orElseGet(Collections::emptyList)
            .stream()
            .map(this::mapRejectionReason)
            .collect(Collectors.toSet());
    String rejectionReasons = String.join(COMMA_DELIMITER, onboardingRejectionReasonsCode);

    if (onboarding != null && !OnboardingWorkflowConstants.DEMANDED.equals(evaluationDTO.getStatus())) {
      onboarding.setFamilyId(evaluationDTO.getFamilyId());
      setStatus(onboarding, evaluationDTO.getStatus(),
              evaluationDTO.getAdmissibilityCheckDate(), rejectionReasons);
      log.info("[COMPLETE_ONBOARDING] [RESULT] The onboarding status of user {} on initiative {} is: {}",
              onboarding.getUserId(), evaluationDTO.getInitiativeId(), evaluationDTO.getStatus());
    }

    if (onboarding == null && OnboardingWorkflowConstants.DEMANDED.equals(evaluationDTO.getStatus())) {
      log.info("[COMPLETE_ONBOARDING] New onboarding for user {} on initiative {} with status DEMANDED",
              evaluationDTO.getUserId(), evaluationDTO.getInitiativeId());
      Onboarding newOnboarding = new Onboarding(evaluationDTO.getInitiativeId(),
              evaluationDTO.getUserId());
      newOnboarding.setStatus(OnboardingWorkflowConstants.DEMANDED);
      LocalDateTime localDateTime = LocalDateTime.now();
      newOnboarding.setDemandedDate(localDateTime);
      newOnboarding.setUpdateDate(localDateTime);
      newOnboarding.setCreationDate(localDateTime);
      newOnboarding.setFamilyId(evaluationDTO.getFamilyId());
      onboardingRepository.save(newOnboarding);
    }

    if (onboarding != null && OnboardingWorkflowConstants.DEMANDED.equals(evaluationDTO.getStatus())) {
      LocalDateTime localDateTime = LocalDateTime.now();
      onboarding.setDemandedDate(localDateTime);
      onboarding.setUpdateDate(localDateTime);
      onboarding.setFamilyId(evaluationDTO.getFamilyId());
      onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
      onboardingRepository.save(onboarding);
      evaluationDTO.setStatus(OnboardingWorkflowConstants.JOINED);
      outcomeProducer.sendOutcome(evaluationDTO);
      log.info("[COMPLETE_ONBOARDING] [RESULT] The onboarding status of user {} on initiative {} is ONBOARDING_OK",
              onboarding.getUserId(), evaluationDTO.getInitiativeId());
    }

    if (onboarding == null && OnboardingWorkflowConstants.ONBOARDING_KO.equals(evaluationDTO.getStatus())) {
      log.info("[COMPLETE_ONBOARDING] New onboarding for user {} on initiative {} with status ONBOARDING_KO",
              evaluationDTO.getUserId(), evaluationDTO.getInitiativeId());
      Onboarding newOnboarding = new Onboarding(evaluationDTO.getInitiativeId(),
              evaluationDTO.getUserId());
      if (rejectionReasons.contains(OnboardingWorkflowConstants.OUT_OF_RANKING)) {
        newOnboarding.setStatus(OnboardingWorkflowConstants.ELIGIBLE_KO);
      } else {
        newOnboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
      }
      newOnboarding.setDetailKO(rejectionReasons);
      LocalDateTime localDateTime = LocalDateTime.now();
      newOnboarding.setUpdateDate(localDateTime);
      newOnboarding.setCreationDate(localDateTime);
      newOnboarding.setFamilyId(evaluationDTO.getFamilyId());
      onboardingRepository.save(newOnboarding);
    }

    performanceLog(startTime, "COMPLETE_ONBOARDING", evaluationDTO.getUserId(),
        evaluationDTO.getInitiativeId());
  }

  @Override
  public void allowedInitiative(OnboardingNotificationDTO onboardingNotificationDTO) {
    long startTime = System.currentTimeMillis();

    log.info("[ALLOWED_INITIATIVE] Consumer onboarding notification");
    if (onboardingNotificationDTO.getOperationType()
        .equals(OnboardingWorkflowConstants.ALLOWED_CITIZEN_PUBLISH)) {
      log.info("[ALLOWED_INITIATIVE] Allowed citizen");
      Onboarding onboarding = onboardingRepository.findById(
              Onboarding.buildId(
                      onboardingNotificationDTO.getInitiativeId(),
                      onboardingNotificationDTO.getUserId()))
              .orElse(null);
      if (onboarding == null) {
        log.info("[ALLOWED_INITIATIVE] New onboarding with status INVITED");
        Onboarding newOnboarding = new Onboarding(onboardingNotificationDTO.getInitiativeId(),
            onboardingNotificationDTO.getUserId());
        newOnboarding.setStatus(OnboardingWorkflowConstants.INVITED);
        LocalDateTime localDateTime = LocalDateTime.now();
        newOnboarding.setInvitationDate(localDateTime);
        newOnboarding.setUpdateDate(localDateTime);
        newOnboarding.setCreationDate(localDateTime);
        onboardingRepository.save(newOnboarding);
      }
    }
    performanceLog(startTime, "ALLOWED_INITIATIVE", onboardingNotificationDTO.getUserId(),
        onboardingNotificationDTO.getInitiativeId());
  }

  @Override
  public void suspend(String initiativeId, String userId){
    long startTime = System.currentTimeMillis();
    log.info("[SUSPENSION] User suspension from the initiative {} started", initiativeId);

    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);
    if (!List.of(OnboardingWorkflowConstants.ONBOARDING_OK,OnboardingWorkflowConstants.SUSPENDED).contains(onboarding.getStatus())){
      auditUtilities.logSuspensionKO(userId, initiativeId);
      performanceLog(startTime, SUSPENSION, userId, initiativeId);
      log.info("[SUSPENSION] User suspension from the initiative {} is not possible", initiativeId);
      throw new OperationNotAllowedException(SUSPENSION_NOT_ALLOWED,
              String.format(ERROR_SUSPENSION_STATUS_MSG, initiativeId));
    }
    try {
      onboarding.setStatus(OnboardingWorkflowConstants.SUSPENDED);
      LocalDateTime updateDate = LocalDateTime.now();
      onboarding.setUpdateDate(updateDate);
      onboarding.setSuspensionDate(updateDate);
      onboardingRepository.save(onboarding);
      auditUtilities.logSuspension(userId, initiativeId);
      log.info("[SUSPENSION] User is suspended from the initiative {}", initiativeId);
      performanceLog(startTime, SUSPENSION, userId, initiativeId);
    } catch (Exception e){
      auditUtilities.logSuspensionKO(userId, initiativeId);
      performanceLog(startTime, SUSPENSION, userId, initiativeId);
      log.info("[SUSPENSION] User suspension from the initiative {} is failed", initiativeId);
      throw new UserSuspensionOrReadmissionException(String.format(ERROR_SUSPENSION_MSG, initiativeId));
    }
  }

  @Override
  public void readmit(String initiativeId, String userId){
    long startTime = System.currentTimeMillis();
    log.info("[READMISSION] User readmission to the initiative {} started", initiativeId);

    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);
    if (!List.of(OnboardingWorkflowConstants.ONBOARDING_OK,OnboardingWorkflowConstants.SUSPENDED).contains(onboarding.getStatus())){
      auditUtilities.logReadmissionKO(userId, initiativeId);
      performanceLog(startTime, READMISSION, userId, initiativeId);
      log.info("[READMISSION] User readmission to the initiative {} is not possible", initiativeId);
      throw new OperationNotAllowedException(READMISSION_NOT_ALLOWED,
              String.format(ERROR_READMIT_STATUS_MSG, initiativeId));
    }
    try {
      onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
      LocalDateTime updateDate = LocalDateTime.now();
      onboarding.setUpdateDate(updateDate);
      onboarding.setSuspensionDate(null);
      onboardingRepository.save(onboarding);
      auditUtilities.logReadmission(userId, initiativeId);
      log.info("[READMISSION] User is readmitted to the initiative {}", initiativeId);
      performanceLog(startTime, READMISSION, userId, initiativeId);
    } catch (Exception e){
      auditUtilities.logReadmissionKO(userId, initiativeId);
      performanceLog(startTime, READMISSION, userId, initiativeId);
      log.info("[READMISSION] User readmission to the initiative {} is failed", initiativeId);
      throw new UserSuspensionOrReadmissionException(String.format(ERROR_READMISSION_MSG, initiativeId));
    }
  }

  @SuppressWarnings("BusyWait")
  @Override
  public void processCommand(QueueCommandOperationDTO queueCommandOperationDTO) {

    if (("DELETE_INITIATIVE").equals(queueCommandOperationDTO.getOperationType())) {
      long startTime = System.currentTimeMillis();

      List<Onboarding> totalDeletedOnboardings = new ArrayList<>();
      List<Onboarding> fetchedOnboardings;

      do {
        fetchedOnboardings = onboardingRepository.deletePaged(queueCommandOperationDTO.getEntityId(),
                pageSize);

        totalDeletedOnboardings.addAll(fetchedOnboardings);

        try {
          Thread.sleep(delayTime);
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
          log.error("An error has occurred while waiting {}", e.getMessage());
        }

      } while (fetchedOnboardings.size() == pageSize);

      log.info("[DELETE_INITIATIVE] Deleted initiative {} from collection: onboarding_citizen", queueCommandOperationDTO.getEntityId());
      totalDeletedOnboardings.forEach(deletedOnboarding -> auditUtilities.logDeletedOnboarding(deletedOnboarding.getUserId(), deletedOnboarding.getInitiativeId()));

      log.info(
              "[PERFORMANCE_LOG] [DELETE_INITIATIVE] Time occurred to perform business logic: {} ms on initiativeId: {}",
              System.currentTimeMillis() - startTime,
              queueCommandOperationDTO.getEntityId());
    }
  }

  private Pageable getPageable(Pageable pageable) {
    if (pageable == null) {
      return PageRequest.of(0, 15, Sort.by("lastUpdate"));
    }
    return pageable;
  }

  @Override
  public OnboardingFamilyDTO getfamilyUnitComposition(String initiativeId, String userId) {
    long startTime = System.currentTimeMillis();

    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);
    List<OnboardingFamilyDetailDTO> usersListDecrypted = new ArrayList<>();
    OnboardingFamilyDTO onboardingFamilyDecryptDTO = new OnboardingFamilyDTO(usersListDecrypted);

    log.info("[GET_ONBOARDING_FAMILY] Retrieved familyId {} for user {}", onboarding.getFamilyId(), userId);

    if(onboarding.getFamilyId() == null){
      performanceLog(startTime, GET_ONBOARDING_FAMILY, userId, initiativeId);
      return onboardingFamilyDecryptDTO;
    }

    List<Onboarding> familyList = onboardingRepository.findByInitiativeIdAndFamilyId(initiativeId, onboarding.getFamilyId());

    usersListDecrypted = familyList.stream().map(o -> {
      LocalDateTime onboardingDate = !OnboardingWorkflowConstants.ONBOARDING_KO.equals(o.getStatus()) ?
              o.getOnboardingOkDate() : o.getOnboardingKODate();
      return new OnboardingFamilyDetailDTO(decryptCF(o.getUserId()), o.getFamilyId(),
              (onboardingDate != null ? onboardingDate.toLocalDate() : null), o.getStatus());
    }).toList();

    onboardingFamilyDecryptDTO.setUsersList(usersListDecrypted);
    performanceLog(startTime, GET_ONBOARDING_FAMILY, userId, initiativeId);
    return onboardingFamilyDecryptDTO;
  }

  private String decryptCF(String userId) {
    String fiscalCode;
    try {
      DecryptCfDTO decryptedCfDTO = decryptRestConnector.getPiiByToken(userId);
      fiscalCode = decryptedCfDTO.getPii();
    } catch (Exception e) {
      throw new PDVInvocationException(PDV_DECRYPT_ERROR_MSG, true, e);
    }
    return fiscalCode;
  }

  private String mapRejectionReason(OnboardingRejectionReason rejectionReason){
    //Remapping the GENERIC_ERROR throw by admissibility for technical issues in a TECHNICAL_ERROR
    if(OnboardingWorkflowConstants.GENERIC_ERROR.equals(rejectionReason.getCode())){
      return OnboardingWorkflowConstants.ERROR_TECHNICAL;
    }

    return rejectionReason.getCode();
  }

}
