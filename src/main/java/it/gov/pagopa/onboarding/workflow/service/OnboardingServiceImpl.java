package it.gov.pagopa.onboarding.workflow.service;

import feign.FeignException;
import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingNotificationDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingRejectionReason;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusCitizenDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.PDNDCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.ResponseInitiativeOnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeGeneralDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.enums.AutomatedCriteria;
import it.gov.pagopa.onboarding.workflow.event.producer.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.event.producer.OutcomeProducer;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import it.gov.pagopa.onboarding.workflow.utils.AuditUtilities;

import it.gov.pagopa.onboarding.workflow.utils.Utilities;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.support.PageableExecutionUtils;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class OnboardingServiceImpl implements OnboardingService {

  public static final String PUT_TC_CONSENT = "PUT_TC_CONSENT";
  public static final String SUSPENSION = "SUSPENSION";
  public static final String READMISSION = "READMISSION";
  public static final String EMPTY = "";
  public static final String COMMA_DELIMITER = ",";
  @Autowired
  private OnboardingRepository onboardingRepository;

  @Autowired
  ConsentMapper consentMapper;

  @Autowired
  OnboardingProducer onboardingProducer;

  @Autowired
  OutcomeProducer outcomeProducer;

  @Autowired
  InitiativeRestConnector initiativeRestConnector;

  @Autowired
  AuditUtilities auditUtilities;

  @Autowired
  Utilities utilities;

  private Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId) {
    return onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
        .orElseThrow(() -> new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
            String.format(OnboardingWorkflowConstants.ID_S_NOT_FOUND, initiativeId), null));
  }

  @Override
  public void putTcConsent(String initiativeId, String userId) {
    long startTime = System.currentTimeMillis();

    InitiativeDTO initiativeDTO = getInitiative(initiativeId);

    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
        .orElse(null);

    if (onboarding != null && !onboarding.getStatus().equals(OnboardingWorkflowConstants.INVITED)) {
      checkStatus(onboarding);

      log.info("[PUT_TC_CONSENT] User has already accepted T&C");
      auditUtilities.logTCIdemp(userId, initiativeId, onboarding.getChannel());
      performanceLog(startTime, PUT_TC_CONSENT);
      return;
    }

    LocalDateTime localDateTime = LocalDateTime.now();

    if (onboarding == null) {
      onboarding = new Onboarding(initiativeId, userId);
      onboarding.setCreationDate(localDateTime);
      onboarding.setUpdateDate(localDateTime);
    }

    checkDates(initiativeDTO, onboarding);
    checkBudget(initiativeDTO, onboarding);

    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);
    onboarding.setTcAcceptTimestamp(localDateTime);
    onboarding.setUpdateDate(localDateTime);
    onboardingRepository.save(onboarding);
    auditUtilities.logTC(userId, initiativeId, onboarding.getChannel());
    performanceLog(startTime, PUT_TC_CONSENT);
  }

  private void setStatus(Onboarding onboarding, String status, LocalDateTime date, String rejectionReasons) {
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
      checkDates(initiativeDTO, onboarding);
      checkBudget(initiativeDTO, onboarding);
    }

    onboarding.setChannel(channel);
    RequiredCriteriaDTO dto = null;

    if (!checkWhitelist(onboarding, initiativeDTO)) {
      dto = getCriteriaLists(initiativeDTO);
    }

    onboardingRepository.save(onboarding);
    auditUtilities.logPDND(userId, initiativeId, onboarding.getChannel());
    performanceLog(startTime, "CHECK_PREREQUISITES");
    return dto;
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
      throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
              OnboardingWorkflowConstants.ERROR_WHITELIST_MSG, OnboardingWorkflowConstants.GENERIC_ERROR);
    }
    setStatus(onboarding, OnboardingWorkflowConstants.ON_EVALUATION, LocalDateTime.now(), null);
    outcomeProducer.sendOutcome(createEvaluationDto(onboarding.getInitiativeId(),
        onboarding.getUserId(), initiativeDTO));
    return true;
  }
  private EvaluationDTO createEvaluationDto(String initiativeId, String userId,
      InitiativeDTO initiativeDTO) {
    EvaluationDTO dto = new EvaluationDTO();
    dto.setInitiativeId(initiativeId);
    dto.setInitiativeName(initiativeDTO.getInitiativeName());
    dto.setInitiativeEndDate(initiativeDTO.getGeneral().getEndDate());
    dto.setUserId(userId);
    dto.setOrganizationId(initiativeDTO.getOrganizationId());
    dto.setAdmissibilityCheckDate(LocalDateTime.now());
    dto.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
    dto.setOnboardingRejectionReasons(List.of());
    dto.setBeneficiaryBudget(initiativeDTO.getGeneral().getBeneficiaryBudget());
    dto.setInitiativeRewardType(initiativeDTO.getInitiativeRewardType());
    return dto;
  }

  private void checkDates(InitiativeDTO initiativeDTO, Onboarding onboarding) {
    LocalDate requestDate = LocalDate.now();

    LocalDate startDate =
        (initiativeDTO.getGeneral().getRankingStartDate() != null) ? initiativeDTO.getGeneral()
            .getRankingStartDate() : initiativeDTO.getGeneral()
            .getStartDate();

    LocalDate endDate =
        (initiativeDTO.getGeneral().getRankingEndDate() != null) ? initiativeDTO.getGeneral()
            .getRankingEndDate() : initiativeDTO.getGeneral()
            .getEndDate();

    if (requestDate.isBefore(startDate)){
      auditUtilities.logOnboardingKOWithReason(onboarding.getInitiativeId(), onboarding.getUserId(), onboarding.getChannel(),
              OnboardingWorkflowConstants.ERROR_INITIATIVE_NOT_STARTED_MSG);
      throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
              OnboardingWorkflowConstants.ERROR_INITIATIVE_NOT_STARTED_MSG,
              OnboardingWorkflowConstants.ERROR_INITIATIVE_NOT_STARTED);
    }

    if (requestDate.isAfter(endDate)){
      LocalDateTime localDateTime = LocalDateTime.now();
      onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
      onboarding.setOnboardingKODate(localDateTime);
      onboarding.setUpdateDate(localDateTime);
      onboarding.setDetailKO(OnboardingWorkflowConstants.ERROR_INITIATIVE_END);
      onboardingRepository.save(onboarding);
      auditUtilities.logOnboardingKOWithReason(onboarding.getUserId(), onboarding.getInitiativeId(), onboarding.getChannel(),
              OnboardingWorkflowConstants.ERROR_INITIATIVE_END_MSG);
      throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
              OnboardingWorkflowConstants.ERROR_INITIATIVE_END_MSG,
              OnboardingWorkflowConstants.ERROR_INITIATIVE_END);
    }
  }

  private void checkBudget(InitiativeDTO initiativeDTO, Onboarding onboarding) {
    InitiativeGeneralDTO generalInfo = initiativeDTO.getGeneral();
    BigDecimal totalBudget = generalInfo.getBudget();
    BigDecimal beneficiaryBudget = generalInfo.getBeneficiaryBudget();
    int onboardedCitizen = onboardingRepository.countByInitiativeIdAndStatus(initiativeDTO.getInitiativeId(),
            OnboardingWorkflowConstants.ONBOARDING_OK) + 1;

    BigDecimal budgetUsed = beneficiaryBudget.multiply(BigDecimal.valueOf(onboardedCitizen));
    if (budgetUsed.compareTo(totalBudget) > 0){
      LocalDateTime localDateTime = LocalDateTime.now();
      onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
      onboarding.setOnboardingKODate(localDateTime);
      onboarding.setUpdateDate(localDateTime);
      onboarding.setDetailKO(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED);
      onboardingRepository.save(onboarding);
      auditUtilities.logOnboardingKOWithReason(onboarding.getInitiativeId(), onboarding.getUserId(), onboarding.getChannel(),
              OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED_MSG);
      throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
              OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED_MSG,
              OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED);
    }
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

  private void checkStatus(Onboarding onboarding){
    String status = onboarding.getStatus();
    if (List.of(OnboardingWorkflowConstants.ONBOARDING_KO, OnboardingWorkflowConstants.ELIGIBLE_KO).contains(status)){
      auditUtilities.logOnboardingKOWithReason(onboarding.getUserId(), onboarding.getInitiativeId(), onboarding.getChannel(),
              utilities.getMessageOnboardingKO(onboarding.getDetailKO()));
      throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
              utilities.getMessageOnboardingKO(onboarding.getDetailKO()),
              onboarding.getDetailKO());
    }
    if (status.equals(OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED)) {
      auditUtilities.logOnboardingKOWithReason(onboarding.getUserId(), onboarding.getInitiativeId(), onboarding.getChannel(),
              OnboardingWorkflowConstants.ERROR_UNSUBSCRIBED_INITIATIVE);
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
              OnboardingWorkflowConstants.ERROR_UNSUBSCRIBED_INITIATIVE,
              OnboardingWorkflowConstants.GENERIC_ERROR);
    }
  }

  private InitiativeDTO getInitiative(String initiativeId) {
    try {
      log.info("[GET_INITIATIVE] Retrieving information for initiative {}", initiativeId);
      InitiativeDTO initiativeDTO = initiativeRestConnector.getInitiativeBeneficiaryView(
          initiativeId);
      log.info(initiativeDTO.toString());
      if (!initiativeDTO.getStatus().equals(OnboardingWorkflowConstants.PUBLISHED)) {
        log.info("[GET_INITIATIVE] Initiative {} is not active PUBLISHED! Status: {}", initiativeId,
            initiativeDTO.getStatus());
        throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
            OnboardingWorkflowConstants.ERROR_INITIATIVE_NOT_ACTIVE, OnboardingWorkflowConstants.GENERIC_ERROR);
      }
      log.info("[GET_INITIATIVE] Initiative {} is PUBLISHED", initiativeId);
      return initiativeDTO;
    } catch (FeignException e) {
      log.error("[GET_INITIATIVE] Initiative {}: something went wrong when invoking the API.",
          initiativeId);
      throw new OnboardingWorkflowException(e.status(), e.contentUTF8(), OnboardingWorkflowConstants.GENERIC_ERROR);
    }
  }

  @Override
  public OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId) {
    long startTime = System.currentTimeMillis();
    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);
    performanceLog(startTime, "GET_ONBOARDING_STATUS");
    return new OnboardingStatusDTO(onboarding.getStatus());
  }

  @Override
  public ResponseInitiativeOnboardingDTO getOnboardingStatusList(String initiativeId,
      String userId, LocalDateTime startDate, LocalDateTime endDate, String status,
      Pageable pageable) {
    long startTime = System.currentTimeMillis();

    if (pageable != null && pageable.getPageSize() > 15) {
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
          OnboardingWorkflowConstants.ERROR_MAX_NUMBER_FOR_PAGE, null);
    }
    List<OnboardingStatusCitizenDTO> onboardingStatusCitizenDTOS = new ArrayList<>();
    Criteria criteria = onboardingRepository.getCriteria(initiativeId, userId, status, startDate,
        endDate);
    List<Onboarding> onboardinglist = onboardingRepository.findByFilter(criteria, pageable);
    long count = onboardingRepository.getCount(criteria);
    final Page<Onboarding> result = PageableExecutionUtils.getPage(onboardinglist,
        this.getPageable(pageable),
        () -> count);
    for (Onboarding o : onboardinglist) {
      OnboardingStatusCitizenDTO onboardingStatusCitizenDTO = new OnboardingStatusCitizenDTO(
          o.getUserId(), o.getStatus(), o.getUpdateDate()!=null?o.getUpdateDate().toString(): EMPTY);
      onboardingStatusCitizenDTOS.add(onboardingStatusCitizenDTO);
    }
    performanceLog(startTime, "GET_ONBOARDING_STATUS_LIST");
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
      performanceLog(startTime, "SAVE_CONSENT");
      auditUtilities.logOnboardingKOWithReason(userId, initiativeDTO.getInitiativeId(), onboarding.getChannel(),
              String.format(OnboardingWorkflowConstants.ERROR_PDND, consentPutDTO.getInitiativeId()));
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
          String.format(OnboardingWorkflowConstants.ERROR_PDND,
              consentPutDTO.getInitiativeId()), null);
    }

    selfDeclaration(initiativeDTO, consentPutDTO);
    onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
    onboarding.setPdndAccept(consentPutDTO.isPdndAccept());
    LocalDateTime localDateTime = LocalDateTime.now();
    onboarding.setCriteriaConsensusTimestamp(localDateTime);
    onboarding.setUpdateDate(localDateTime);
    OnboardingDTO onboardingDTO = consentMapper.map(onboarding);
    onboardingProducer.sendSaveConsent(onboardingDTO);
    onboardingRepository.save(onboarding);
    performanceLog(startTime, "SAVE_CONSENT");
  }

  private void selfDeclaration(InitiativeDTO initiativeDTO, ConsentPutDTO consentPutDTO) {
    if (initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria().isEmpty()) {
      return;
    }

    Map<String, Boolean> selfDeclarationBool = consentPutDTO.getSelfDeclarationList().stream()
        .filter(item -> item.getClass().equals(SelfConsentBoolDTO.class))
        .map(SelfConsentBoolDTO.class::cast)
        .collect(Collectors.toMap(SelfConsentBoolDTO::getCode, SelfConsentBoolDTO::isAccepted));

    Map<String, String> selfDeclarationMulti = consentPutDTO.getSelfDeclarationList().stream()
        .filter(item -> item.getClass().equals(SelfConsentMultiDTO.class))
        .map(SelfConsentMultiDTO.class::cast)
        .collect(Collectors.toMap(SelfConsentMultiDTO::getCode, SelfConsentMultiDTO::getValue));

    if (selfDeclarationBool.size() + selfDeclarationMulti.size()
        != initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria().size()) {
      auditUtilities.logOnboardingKOInitiativeId(initiativeDTO.getInitiativeId(), OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_SIZE);
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
          OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_SIZE, null);
    }

    initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria().forEach(item -> {
      if (item instanceof SelfCriteriaBoolDTO bool) {
        Boolean flag = selfDeclarationBool.get(bool.getCode());
        if (flag == null || !flag) {
          auditUtilities.logOnboardingKOInitiativeId(initiativeDTO.getInitiativeId(),
                  String.format(OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY, consentPutDTO.getInitiativeId()));
          throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
              String.format(OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY,
                  consentPutDTO.getInitiativeId()), null);

        }
        bool.setValue(true);
      }
      if (item instanceof SelfCriteriaMultiDTO multi) {
        String value = selfDeclarationMulti.get(multi.getCode());
        if (value == null || !multi.getValue().contains(value)) {
          auditUtilities.logOnboardingKOInitiativeId(initiativeDTO.getInitiativeId(),
                  String.format(OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY, consentPutDTO.getInitiativeId()));
          throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
              String.format(OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY,
                  consentPutDTO.getInitiativeId()), null);
        }
        multi.setValue(List.of(value));
      }
    });
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
    performanceLog(startTime, "DEACTIVATE_ONBOARDING");
  }

  @Override
  public void rollback(String initiativeId, String userId) {
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
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

    Set<String> onboardingRejectionReasonsType = Optional.ofNullable(evaluationDTO.getOnboardingRejectionReasons())
            .orElseGet(Collections::emptyList)
            .stream()
            .map(OnboardingRejectionReason::getType)
            .collect(Collectors.toSet());
    String rejectionReasons = String.join(COMMA_DELIMITER, onboardingRejectionReasonsType);

    onboardingRepository.findByInitiativeIdAndUserId(evaluationDTO.getInitiativeId(),
                    evaluationDTO.getUserId())
            .ifPresent(onboarding ->
                    setStatus(onboarding, evaluationDTO.getStatus(),
                            evaluationDTO.getAdmissibilityCheckDate(), rejectionReasons)
            );
    log.info("[COMPLETE_ONBOARDING] [RESULT] The onboarding's status is: {}", evaluationDTO.getStatus());
    performanceLog(startTime, "COMPLETE_ONBOARDING");
  }

  @Override
  public void allowedInitiative(OnboardingNotificationDTO onboardingNotificationDTO) {
    long startTime = System.currentTimeMillis();

    log.info("[ALLOWED_INITIATIVE] Consumer onboarding notification");
    if (onboardingNotificationDTO.getOperationType()
        .equals(OnboardingWorkflowConstants.ALLOWED_CITIZEN_PUBLISH)) {
      log.info("[ALLOWED_INITIATIVE] Allowed citizen");
      Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(
          onboardingNotificationDTO.getInitiativeId(),
          onboardingNotificationDTO.getUserId()).orElse(null);
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
    performanceLog(startTime, "ALLOWED_INITIATIVE");
  }
  @Override
  public void suspend(String initiativeId, String userId){
    long startTime = System.currentTimeMillis();
    log.info("[SUSPENSION] User suspension from the initiative {} started", initiativeId);

    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);
    if (!List.of(OnboardingWorkflowConstants.ONBOARDING_OK,OnboardingWorkflowConstants.SUSPENDED).contains(onboarding.getStatus())){
      auditUtilities.logSuspensionKO(userId, initiativeId);
      performanceLog(startTime, SUSPENSION);
      log.info("[SUSPENSION] User suspension from the initiative {} is not possible", initiativeId);
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
              OnboardingWorkflowConstants.ERROR_SUSPENSION_STATUS, null);
    }
    try {
      onboarding.setStatus(OnboardingWorkflowConstants.SUSPENDED);
      LocalDateTime updateDate = LocalDateTime.now();
      onboarding.setUpdateDate(updateDate);
      onboarding.setSuspensionDate(updateDate);
      onboardingRepository.save(onboarding);
      auditUtilities.logSuspension(userId, initiativeId);
      log.info("[SUSPENSION] User is suspended from the initiative {}", initiativeId);
      performanceLog(startTime, SUSPENSION);
    } catch (Exception e){
      auditUtilities.logSuspensionKO(userId, initiativeId);
      performanceLog(startTime, SUSPENSION);
      log.info("[SUSPENSION] User suspension from the initiative {} is failed", initiativeId);
      throw new OnboardingWorkflowException(HttpStatus.INTERNAL_SERVER_ERROR.value(),
              OnboardingWorkflowConstants.ERROR_SUSPENSION, OnboardingWorkflowConstants.GENERIC_ERROR);
    }
  }

  @Override
  public void readmit(String initiativeId, String userId){
    long startTime = System.currentTimeMillis();
    log.info("[READMISSION] User readmission to the initiative {} started", initiativeId);

    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);
    if (!List.of(OnboardingWorkflowConstants.ONBOARDING_OK,OnboardingWorkflowConstants.SUSPENDED).contains(onboarding.getStatus())){
      auditUtilities.logReadmissionKO(userId, initiativeId);
      performanceLog(startTime, READMISSION);
      log.info("[READMISSION] User readmission to the initiative {} is not possible", initiativeId);
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
              OnboardingWorkflowConstants.ERROR_READMIT_STATUS, null);
    }
    try {
      onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
      LocalDateTime updateDate = LocalDateTime.now();
      onboarding.setUpdateDate(updateDate);
      onboarding.setSuspensionDate(null);
      onboardingRepository.save(onboarding);
      auditUtilities.logReadmission(userId, initiativeId);
      log.info("[READMISSION] User is readmitted to the initiative {}", initiativeId);
      performanceLog(startTime, READMISSION);
    } catch (Exception e){
      auditUtilities.logReadmissionKO(userId, initiativeId);
      performanceLog(startTime, READMISSION);
      log.info("[READMISSION] User readmission to the initiative {} is failed", initiativeId);
      throw new OnboardingWorkflowException(HttpStatus.INTERNAL_SERVER_ERROR.value(),
              OnboardingWorkflowConstants.ERROR_READMISSION, OnboardingWorkflowConstants.GENERIC_ERROR);
    }
  }


  private Pageable getPageable(Pageable pageable) {
    if (pageable == null) {
      return PageRequest.of(0, 15, Sort.by("lastUpdate"));
    }
    return pageable;
  }

  private void performanceLog(long startTime, String service){
    log.info(
        "[PERFORMANCE_LOG] [{}] Time occurred to perform business logic: {} ms",
        service,
        System.currentTimeMillis() - startTime);
  }
}
