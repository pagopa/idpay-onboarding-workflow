package it.gov.pagopa.onboarding.workflow.service;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import it.gov.pagopa.onboarding.workflow.dto.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.PDNDCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfDeclarationDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.dto.mapper.producer.SaveConsentDTO;
import it.gov.pagopa.onboarding.workflow.event.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

@Service
public class OnboardingServiceImpl implements OnboardingService {

  @Autowired
  private OnboardingRepository onboardingRepository;

  @Autowired
  ConsentMapper consentMapper;

  @Autowired
  OnboardingProducer onboardingProducer;

  private static final Logger LOG = LoggerFactory.getLogger(
      OnboardingServiceImpl.class);


  @Override
  public Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId) {
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

  @Override
  public void setStatus(Onboarding onboarding, String status) {
    onboarding.setStatus(status);
    onboardingRepository.save(onboarding);
  }

  @Override
  public void checkPrerequisites(String initiativeId) { // Integrare con il sottosistema iniziativa

    boolean check = initiativeId.equals("123");
    if (!check) {
      throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
          String.format("The initiative with id %s has not met the prerequisites.", initiativeId));
    }
  }

  @Override
  public boolean checkCFWhitelist(String initiativeId,
      String userId) { // Integrare con il sottosistema iniziativa
    return userId.equals("123");
  }

  @Override
  public RequiredCriteriaDTO getCriteriaLists(
      String initiativeId) { // Integrare con il sottosistema iniziativa
    PDNDCriteriaDTO pdndCriteriaDTO = new PDNDCriteriaDTO("0", "test_descrizione_pdnd",
        "test_ente_pdnd");
    SelfDeclarationDTO selfDeclarationDTO = new SelfDeclarationDTO("0",
        "test_descrizione_selfDeclaration");
    SelfDeclarationDTO selfDeclarationDTO1 = new SelfDeclarationDTO("01",
        "test_descrizione_selfDeclaration");
    List<PDNDCriteriaDTO> pdndCriteria = new ArrayList<>();
    pdndCriteria.add(pdndCriteriaDTO);
    List<SelfDeclarationDTO> selfDeclarationList = new ArrayList<>();
    selfDeclarationList.add(selfDeclarationDTO);
    selfDeclarationList.add(selfDeclarationDTO1);
    return new RequiredCriteriaDTO(pdndCriteria, selfDeclarationList);
  }

  @Override
  public void checkTCStatus(Onboarding onboarding) {
    if (!onboarding.getStatus()
        .equals(OnboardingWorkflowConstants.ACCEPTED_TC)) {
      throw new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
          String.format("Terms and Conditions have been not accepted by user %s for initiative %s.",
              onboarding.getUserId(), onboarding.getInitiativeId()));
    }
  }

  private void getInitiative(String initiativeId) { // Integrare con il sottosistema iniziativa
    boolean check = initiativeId.equals("123") || initiativeId.contains("francesco");
    if (!check) {
      throw new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
          String.format("The initiative with id %s does not exist.", initiativeId));
    }
  }

  @Override
  public OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId) {
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
        .orElseThrow(() -> new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
            String.format(OnboardingWorkflowConstants.ID_S_NOT_FOUND, initiativeId,
                userId)));
    return new OnboardingStatusDTO(onboarding.getStatus());
  }


  @Override
  public void saveConsent(ConsentPutDTO consentPutDTO, String userId) {
    Onboarding onboarding = findByInitiativeIdAndUserId(consentPutDTO.getInitiativeId(), userId);
    InitiativeDTO initiativeDTO = new InitiativeDTO();

    if (initiativeDTO.getPdndCriteria().isEmpty() || consentPutDTO.isPdndAccept()) {
      onboarding.setSelfDeclarationList(selfDeclaration(consentPutDTO));
      onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
      onboarding.setPdndAccept(consentPutDTO.isPdndAccept());
      onboarding.setCriteriaConsensusTimestamp(LocalDateTime.now());
      checkPrerequisites(consentPutDTO.getInitiativeId());
      SaveConsentDTO saveConsentDTO = consentMapper.map(onboarding);
      onboardingProducer.sendSaveConsent(saveConsentDTO);
      onboardingRepository.save(onboarding);
    } else {
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
          String.format(
              "The PDND consense was denied by the user %s for the initiative %s.",
              userId, consentPutDTO.getInitiativeId()));
    }
  }

  @Override
  public Map<String, Boolean> selfDeclaration(ConsentPutDTO consentPutDTO) {
    List<SelfConsentDTO> declarationList = consentPutDTO.getSelfDeclarationList();

    InitiativeDTO initiativeDTO = new InitiativeDTO();

    List<PDNDCriteriaDTO> pdndCriteriaDTOS = new ArrayList<>();
    List<SelfDeclarationDTO> selfDeclarationDTOS = new ArrayList<>();
    InitiativeDTO initiativeDTO1 = new InitiativeDTO(pdndCriteriaDTOS, selfDeclarationDTOS);

    Map<String, Boolean> selfDeclarationMap = new HashMap<>();

    Map<String, Boolean> selfDeclaration = declarationList.stream()
        .collect(Collectors.toMap(SelfConsentDTO::getCode, SelfConsentDTO::isAccepted));

    if (initiativeDTO1.getSelfDeclarationList().isEmpty() && declarationList.isEmpty()) {
      return selfDeclarationMap;
    }
    if (declarationList.size() != initiativeDTO.getSelfDeclarationList().size()) {
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
          "The amount of self declaration lists mismatch the amount of flags");
    }

    for (SelfDeclarationDTO initiativeList : initiativeDTO.getSelfDeclarationList()) {
      Boolean flag = selfDeclaration.get(initiativeList.getCode());
      if (flag != null && flag) {
        selfDeclarationMap.put(initiativeList.getCode(), true);
      } else {
        throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
            String.format(
                "The selfDeclarationList was denied by the user for the initiative %s.",
                consentPutDTO.getInitiativeId()));

      }
    }

    return selfDeclarationMap;
  }

  @Override
  public void deactivateOnboarding(String initiativeId, String userId, String deactivationDate) {
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(
        initiativeId, userId).orElseThrow(
        () -> new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
            OnboardingWorkflowConstants.ID_S_NOT_FOUND));

    onboarding.setStatus(OnboardingWorkflowConstants.STATUS_INACTIVE);
    onboarding.setDeactivationDate(LocalDateTime.parse(deactivationDate));
    onboardingRepository.save(onboarding);
    LOG.info("Onboarding disabled, date: {}", deactivationDate);
  }

  @Override
  public void rollback(String initiativeId, String userId) {
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
        .orElse(null);
    if (onboarding != null && onboarding.getStatus()
        .equals(OnboardingWorkflowConstants.STATUS_INACTIVE)) {
      LOG.info("Onboarding before rollback: {}", onboarding);
      onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
      onboarding.setDeactivationDate(null);
      onboardingRepository.save(onboarding);
      LOG.info("Onboarding after rollback: {}", onboarding);
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
