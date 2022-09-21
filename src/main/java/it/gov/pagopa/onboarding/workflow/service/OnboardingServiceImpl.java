package it.gov.pagopa.onboarding.workflow.service;

import feign.FeignException;
import it.gov.pagopa.onboarding.workflow.connector.GroupRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.PDNDCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.SaveConsentDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfDeclarationDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.CitizenStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfDeclarationItemsDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.event.producer.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
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
  InitiativeRestConnector initiativeRestConnector;

  @Autowired
  GroupRestConnector groupRestConnector;

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
    if (onboarding == null) {
      Onboarding newOnboarding = new Onboarding(initiativeId, userId);
      newOnboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
      newOnboarding.setTcAcceptTimestamp(LocalDateTime.now());
      newOnboarding.setTc(true);
      onboardingRepository.save(newOnboarding);
      return;
    }
    if (onboarding.getStatus().equals(OnboardingWorkflowConstants.STATUS_INACTIVE)) {
      throw new OnboardingWorkflowException(400, "Unsubscribed to initiative");
    }

  }

  private void setStatus(Onboarding onboarding, String status) {
    onboarding.setStatus(status);
    onboardingRepository.save(onboarding);
  }

  @Override
  public RequiredCriteriaDTO checkPrerequisites(String initiativeId, String userId) {
    InitiativeDTO initiativeDTO = getInitiative(initiativeId);
    Onboarding onboarding = findByInitiativeIdAndUserId(initiativeId, userId);
    checkTCStatus(onboarding);

    checkDates(initiativeDTO);
    RequiredCriteriaDTO dto = null;

    if (!checkWhitelist(onboarding, initiativeDTO.getGeneral().getBeneficiaryKnown())) {
      dto = getCriteriaLists(onboarding, initiativeDTO);
      onboarding.setPdndCheck(!initiativeDTO.getBeneficiaryRule().getAutomatedCriteria().isEmpty());
      onboarding.setAutocertificationCheck(
          !initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria().isEmpty());
      onboardingRepository.save(onboarding);
    }
    return dto;
  }

  private boolean checkWhitelist(Onboarding onboarding, boolean beneficiaryKnown) {
    if (!beneficiaryKnown) {
      return false;
    }
    try {
      CitizenStatusDTO citizenStatus = groupRestConnector.getCitizenStatus(
          onboarding.getInitiativeId(), onboarding.getUserId());
      if (!citizenStatus.isStatus()) {
        return false;
      }
      setStatus(onboarding, OnboardingWorkflowConstants.ON_EVALUATION);
      return true;
    } catch (FeignException e) {
      throw new OnboardingWorkflowException(e.status(), e.contentUTF8());
    }
  }

  private void checkDates(InitiativeDTO initiativeDTO) {

    LocalDate requestDate = LocalDate.now();

    boolean dateCheckFail =
        requestDate.isBefore(initiativeDTO.getGeneral()
            .getStartDate()) && requestDate.isAfter(initiativeDTO.getGeneral().getEndDate());

    if (dateCheckFail) {
      throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
          OnboardingWorkflowConstants.ERROR_PREREQUISITES);
    }
  }

  private RequiredCriteriaDTO getCriteriaLists(
      Onboarding onboarding, InitiativeDTO initiativeDTO) {

    RequiredCriteriaDTO requiredCriteriaDTO = new RequiredCriteriaDTO();
    List<SelfDeclarationDTO> selfDeclarationList = new ArrayList<>();
    List<PDNDCriteriaDTO> pdndCriteria = new ArrayList<>();
    List<SelfDeclarationItemsDTO> selfDeclarationListDB = new ArrayList<>();

    initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria().forEach(item -> {
      if (item instanceof SelfCriteriaBoolDTO bool) {
        selfDeclarationList.add(new SelfDeclarationDTO(bool.getCode(), bool.getDescription()));
      } else if (item instanceof SelfCriteriaMultiDTO multi) {
        selfDeclarationList.add(
            new SelfDeclarationDTO(multi.getCode(), multi.getDescription()));
      }
      selfDeclarationListDB.add(item);
    });

    initiativeDTO.getBeneficiaryRule().getAutomatedCriteria().forEach(item ->
        pdndCriteria.add(new PDNDCriteriaDTO(item.getCode(), item.getField(), item.getAuthority()))
    );

    onboarding.setSelfDeclarationList(selfDeclarationListDB);
    requiredCriteriaDTO.setSelfDeclarationList(selfDeclarationList);
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
        throw new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
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
    onboarding.setCriteriaConsensusTimestamp(LocalDateTime.now());
    InitiativeDTO initiativeDTO = getInitiative(consentPutDTO.getInitiativeId());
    checkDates(initiativeDTO);
    SaveConsentDTO saveConsentDTO = consentMapper.map(onboarding);
    onboardingProducer.sendSaveConsent(saveConsentDTO);
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
      onboardingRepository.save(onboarding);
      log.info("Onboarding after rollback: {}", onboarding);
    }

  }

  @Override
  public void completeOnboarding(EvaluationDTO evaluationDTO) {
    onboardingRepository.findByInitiativeIdAndUserId(
        evaluationDTO.getInitiativeId(), evaluationDTO.getUserId()).ifPresent(onboarding ->
        setStatus(onboarding, evaluationDTO.getStatus())
    );
  }
}
