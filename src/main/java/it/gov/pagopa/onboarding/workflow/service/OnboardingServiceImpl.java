package it.gov.pagopa.onboarding.workflow.service;

import feign.FeignException;
import it.gov.pagopa.onboarding.workflow.connector.GroupRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingNotificationDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusCitizenDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.PDNDCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.ResponseInitiativeOnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.CitizenStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfDeclarationItemsDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.event.producer.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.event.producer.OutcomeProducer;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import it.gov.pagopa.onboarding.workflow.utils.Utilities;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
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
  GroupRestConnector groupRestConnector;

  @Autowired
  Utilities utilities;

  private Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId) {
    return onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
        .orElseThrow(() -> new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
            String.format(OnboardingWorkflowConstants.ID_S_NOT_FOUND, initiativeId,
                userId)));
  }

  @Override
  public void putTcConsent(String initiativeId, String userId) {
    getInitiative(initiativeId);
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
        .orElse(null);
    if (onboarding == null || onboarding.getStatus().equals(OnboardingWorkflowConstants.INVITED)) {

      onboarding = new Onboarding(initiativeId, userId);
      onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
      onboarding.setTc(true);
      LocalDateTime localDateTime = LocalDateTime.now();
      onboarding.setTcAcceptTimestamp(localDateTime);
      onboarding.setUpdateDate(localDateTime);
      if (onboarding.getCreationDate() == null) {
        onboarding.setCreationDate(localDateTime);
      }
      onboardingRepository.save(onboarding);
      utilities.logTC(userId, initiativeId);
      return;
    }
    if (onboarding.getStatus().equals(OnboardingWorkflowConstants.STATUS_INACTIVE)) {
      throw new OnboardingWorkflowException(400, "Unsubscribed to initiative");
    }
  }

  private void setStatus(Onboarding onboarding, String status, LocalDateTime date) {
    onboarding.setStatus(status);
    if (status.equals(OnboardingWorkflowConstants.ONBOARDING_OK)) {
      onboarding.setOnboardingOkDate(date);
    }
    if (status.equals(OnboardingWorkflowConstants.ON_EVALUATION)) {
      onboarding.setCriteriaConsensusTimestamp(date);
    }
    onboarding.setUpdateDate(date);
    onboardingRepository.save(onboarding);
  }

  @Override
  public RequiredCriteriaDTO checkPrerequisites(String initiativeId, String userId) {
    InitiativeDTO initiativeDTO = getInitiative(initiativeId);
    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);
    checkTCStatus(onboarding);

    checkDates(initiativeDTO);
    RequiredCriteriaDTO dto = null;

    if (!checkWhitelist(onboarding, initiativeDTO)) {
      dto = getCriteriaLists(onboarding, initiativeDTO);
      onboarding.setPdndCheck(!initiativeDTO.getBeneficiaryRule().getAutomatedCriteria().isEmpty());
      onboarding.setAutocertificationCheck(
          !initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria().isEmpty());
      onboardingRepository.save(onboarding);
      utilities.logPDND(userId,initiativeId);
    }
    return dto;
  }

  private boolean checkWhitelist(Onboarding onboarding, InitiativeDTO initiativeDTO) {
    if (Boolean.FALSE.equals(initiativeDTO.getGeneral().getBeneficiaryKnown())) {
      return false;
    }
    try {
      CitizenStatusDTO citizenStatus = groupRestConnector.getCitizenStatus(
          onboarding.getInitiativeId(), onboarding.getUserId());
      if (!citizenStatus.isStatus()) {
        onboardingRepository.delete(onboarding);
        throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
            OnboardingWorkflowConstants.ERROR_WHITELIST);
      }
      setStatus(onboarding, OnboardingWorkflowConstants.ON_EVALUATION, LocalDateTime.now());
      outcomeProducer.sendOutcome(
          EvaluationDTO.builder()
              .initiativeId(onboarding.getInitiativeId())
              .initiativeName(initiativeDTO.getInitiativeName())
              .initiativeEndDate(initiativeDTO.getGeneral().getEndDate().atStartOfDay())
              .userId(onboarding.getUserId())
              .organizationId(initiativeDTO.getOrganizationId())
              .admissibilityCheckDate(LocalDateTime.now())
              .status(OnboardingWorkflowConstants.ONBOARDING_OK)
              .onboardingRejectionReasons(List.of())
              .beneficiaryBudget(initiativeDTO.getGeneral().getBeneficiaryBudget())
              .serviceId(initiativeDTO.getAdditionalInfo().getServiceId())
              .build());
      return true;
    } catch (FeignException e) {
      throw new OnboardingWorkflowException(e.status(), e.contentUTF8());
    }
  }

  private void checkDates(InitiativeDTO initiativeDTO) {

    LocalDate requestDate = LocalDate.now();

    LocalDate startDate =
        (initiativeDTO.getGeneral().getRankingStartDate() != null) ? initiativeDTO.getGeneral()
            .getRankingStartDate() : initiativeDTO.getGeneral()
            .getStartDate();

    LocalDate endDate =
        (initiativeDTO.getGeneral().getRankingEndDate() != null) ? initiativeDTO.getGeneral()
            .getRankingEndDate() : initiativeDTO.getGeneral()
            .getEndDate();

    boolean dateCheckFail =
        requestDate.isBefore(startDate) || requestDate.isAfter(
            endDate);

    if (dateCheckFail) {
      throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
          OnboardingWorkflowConstants.ERROR_PREREQUISITES);
    }
  }

  private RequiredCriteriaDTO getCriteriaLists(
      Onboarding onboarding, InitiativeDTO initiativeDTO) {

    RequiredCriteriaDTO requiredCriteriaDTO = new RequiredCriteriaDTO();
    List<PDNDCriteriaDTO> pdndCriteria = new ArrayList<>();

    initiativeDTO.getBeneficiaryRule().getAutomatedCriteria().forEach(item ->
        pdndCriteria.add(new PDNDCriteriaDTO(item.getCode(), item.getField(), item.getAuthority()))
    );

    onboarding.setSelfDeclarationList(
        initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria());
    requiredCriteriaDTO.setSelfDeclarationList(
        initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria());
    requiredCriteriaDTO.setPdndCriteria(pdndCriteria);
    return requiredCriteriaDTO;
  }

  private void checkTCStatus(Onboarding onboarding) {
    if (!onboarding.getStatus()
        .equals(OnboardingWorkflowConstants.ACCEPTED_TC)) {
      throw new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
          String.format("Terms and Conditions have been not accepted by user %s for initiative %s.",
              onboarding.getUserId(), onboarding.getInitiativeId()));
    }
  }

  private InitiativeDTO getInitiative(String initiativeId) {
    try {
      InitiativeDTO initiativeDTO = initiativeRestConnector.getInitiativeBeneficiaryView(
          initiativeId);
      if (!initiativeDTO.getStatus().equals("PUBLISHED")) {
        throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
            "The initiative is not active!");
      }
      return initiativeDTO;
    } catch (FeignException e) {
      throw new OnboardingWorkflowException(e.status(), e.contentUTF8());
    }
  }

  @Override
  public OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId) {
    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);
    return new OnboardingStatusDTO(onboarding.getStatus());
  }

  @Override
  public ResponseInitiativeOnboardingDTO getOnboardingStatusList(String initiativeId,
      String userId, LocalDateTime startDate, LocalDateTime endDate, String status,
      Pageable pageable) {
    if (pageable != null && pageable.getPageSize() > 15) {
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
          "Max number for page allowed: 15");
    }
    List<OnboardingStatusCitizenDTO> onboardingStatusCitizenDTOS = new ArrayList<>();
    Criteria criteria = onboardingRepository.getCriteria(initiativeId, userId, status, startDate,
        endDate);
    List<Onboarding> onboardinglist = onboardingRepository.findByFilter(criteria,pageable);
    long count = onboardingRepository.getCount(criteria);
    final Page<Onboarding> result = PageableExecutionUtils.getPage(onboardinglist, this.getPageable(pageable),
        () -> count);
    for (Onboarding o : onboardinglist) {
      OnboardingStatusCitizenDTO onboardingStatusCitizenDTO = new OnboardingStatusCitizenDTO(
          o.getUserId(), o.getStatus(), o.getUpdateDate().toString());
      onboardingStatusCitizenDTOS.add(onboardingStatusCitizenDTO);
    }
    return new ResponseInitiativeOnboardingDTO(onboardingStatusCitizenDTOS, result.getNumber(),
        result.getSize(), result.getNumberOfElements(), result.getTotalPages());
  }

  @Override
  public void saveConsent(ConsentPutDTO consentPutDTO, String userId) {
    Onboarding onboarding = findByInitiativeIdAndUserId(consentPutDTO.getInitiativeId(), userId);
    checkTCStatus(onboarding);

    if (Boolean.TRUE.equals(onboarding.getPdndCheck()) && !consentPutDTO.isPdndAccept()) {
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
          String.format(
              "The PDND consense was denied by the user %s for the initiative %s.",
              userId, consentPutDTO.getInitiativeId()));
    }

    selfDeclaration(onboarding, consentPutDTO);
    onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
    onboarding.setPdndAccept(consentPutDTO.isPdndAccept());
    LocalDateTime localDateTime = LocalDateTime.now();
    onboarding.setCriteriaConsensusTimestamp(localDateTime);
    onboarding.setUpdateDate(localDateTime);
    InitiativeDTO initiativeDTO = getInitiative(consentPutDTO.getInitiativeId());
    checkDates(initiativeDTO);
    OnboardingDTO onboardingDTO = consentMapper.map(onboarding);
    onboardingProducer.sendSaveConsent(onboardingDTO);
    onboardingRepository.save(onboarding);
  }

  private void selfDeclaration(Onboarding onboarding, ConsentPutDTO consentPutDTO) {
    if (Boolean.FALSE.equals(onboarding.getAutocertificationCheck())) {
      return;
    }

    List<SelfDeclarationItemsDTO> listFromDb = onboarding.getSelfDeclarationList();

    Map<String, Boolean> selfDeclarationBool = consentPutDTO.getSelfDeclarationList().stream()
        .filter(item -> item.getClass().equals(SelfConsentBoolDTO.class))
        .map(SelfConsentBoolDTO.class::cast)
        .collect(Collectors.toMap(SelfConsentBoolDTO::getCode, SelfConsentBoolDTO::isAccepted));

    Map<String, String> selfDeclarationMulti = consentPutDTO.getSelfDeclarationList().stream()
        .filter(item -> item.getClass().equals(SelfConsentMultiDTO.class))
        .map(SelfConsentMultiDTO.class::cast)
        .collect(Collectors.toMap(SelfConsentMultiDTO::getCode, SelfConsentMultiDTO::getValue));

    if (selfDeclarationBool.size() + selfDeclarationMulti.size() != listFromDb.size()) {
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
          OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_SIZE);
    }

    listFromDb.forEach(item -> {
      if (item instanceof SelfCriteriaBoolDTO bool) {
        Boolean flag = selfDeclarationBool.get(bool.getCode());
        if (flag == null || !flag) {
          throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
              String.format(
                  OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY,
                  consentPutDTO.getInitiativeId()));

        }
        bool.setValue(true);
      }
      if (item instanceof SelfCriteriaMultiDTO multi) {
        String value = selfDeclarationMulti.get(multi.getCode());
        if (value == null || !multi.getValue().contains(value)) {
          throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
              String.format(
                  OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY,
                  consentPutDTO.getInitiativeId()));

        }
        multi.setValue(List.of(value));
      }
    });
  }

  @Override
  public void deactivateOnboarding(String initiativeId, String userId, String deactivationDate) {
    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);

    onboarding.setStatus(OnboardingWorkflowConstants.STATUS_INACTIVE);
    onboarding.setRequestDeactivationDate(LocalDateTime.parse(deactivationDate));
    onboarding.setUpdateDate(LocalDateTime.parse(deactivationDate));
    onboardingRepository.save(onboarding);
    log.info("Onboarding disabled, date: {}", deactivationDate);
  }

  @Override
  public void rollback(String initiativeId, String userId) {
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
        .orElse(null);
    if (onboarding != null && onboarding.getStatus()
        .equals(OnboardingWorkflowConstants.STATUS_INACTIVE)) {
      log.info("Onboarding before rollback: {}", onboarding);
      onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
      onboarding.setRequestDeactivationDate(null);
      onboarding.setUpdateDate(onboarding.getOnboardingOkDate());
      onboardingRepository.save(onboarding);
      log.info("Onboarding after rollback: {}", onboarding);
    }

  }

  @Override
  public void completeOnboarding(EvaluationDTO evaluationDTO) {
    onboardingRepository.findByInitiativeIdAndUserId(
        evaluationDTO.getInitiativeId(), evaluationDTO.getUserId()).ifPresent(onboarding ->
        setStatus(onboarding, evaluationDTO.getStatus(), evaluationDTO.getAdmissibilityCheckDate())
    );
    utilities.logOnboardingOk(evaluationDTO.getUserId(), evaluationDTO.getInitiativeId());
  }

  @Override
  public void allowedInitiative(OnboardingNotificationDTO onboardingNotificationDTO) {
    log.info("consumer onboarding notification");
    if (onboardingNotificationDTO.getOperationType()
        .equals(OnboardingWorkflowConstants.ALLOWED_CITIZEN_PUBLISH)) {
      log.info("allowed citizen");
      Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(
          onboardingNotificationDTO.getInitiativeId(),
          onboardingNotificationDTO.getUserId()).orElse(null);
      if (onboarding == null) {
        log.info("new onbording with status invited");
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
  }
  private Pageable getPageable(Pageable pageable) {
    if (pageable == null) {
      return PageRequest.of(0, 15, Sort.by("lastUpdate"));
    }
    return pageable;
  }
}
