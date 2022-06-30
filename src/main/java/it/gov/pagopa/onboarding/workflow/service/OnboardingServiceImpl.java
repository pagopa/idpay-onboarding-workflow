package it.gov.pagopa.onboarding.workflow.service;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.PDNDCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfDeclarationDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.dto.mapper.producerDto.SaveConsentDTO;
import it.gov.pagopa.onboarding.workflow.event.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

@Service
public class OnboardingServiceImpl implements OnboardingService {

  private static final String ID_S_NOT_FOUND = "Onboarding with initiativeId %s and userId %s not found.";

  @Autowired
  private OnboardingRepository onboardingRepository;

  @Autowired
  ConsentMapper consentMapper;

  @Autowired
  OnboardingProducer onboardingProducer;


  @Override
  public Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId) {
    return onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
        .orElseThrow(() -> new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
            String.format(ID_S_NOT_FOUND, initiativeId,
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
    }

  }

  @Override
  public void setOnEvaluation(Onboarding onboarding) {
    onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
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
  public RequiredCriteriaDTO getCriteriaLists(String initiativeId) { // Integrare con il sottosistema iniziativa
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
    boolean check = initiativeId.equals("123");
    if (!check) {
      throw new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
          String.format("The initiative with id %s does not exist.", initiativeId));
    }
  }
  @Override
  public OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId) {
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
        .orElseThrow(() -> new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
            String.format(ID_S_NOT_FOUND, initiativeId,
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
      Onboarding onboarding1 = onboardingRepository.save(onboarding);

      SaveConsentDTO saveConsentDTO = consentMapper.map(onboarding1);
      onboardingProducer.sendSaveConsent(saveConsentDTO);
    } else {
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
          String.format(
              "The PDND consense was denied by the user %s for the initiative %s.",
              userId, consentPutDTO.getInitiativeId()));
    }
  }
  @Override
  public List<Boolean> selfDeclaration(ConsentPutDTO consentPutDTO) {
    List<Boolean> flagList = new ArrayList<>();
    List<SelfConsentDTO> declarationList = consentPutDTO.getSelfDeclarationList();
    InitiativeDTO initiativeDTO = new InitiativeDTO();

    List<PDNDCriteriaDTO> pdndCriteriaDTOS = new ArrayList<>();
    List<SelfDeclarationDTO> selfDeclarationDTOS = new ArrayList<>();
    InitiativeDTO initiativeDTO1 = new InitiativeDTO(pdndCriteriaDTOS, selfDeclarationDTOS);

    Map<String, Boolean> list = declarationList.stream()
        .collect(Collectors.toMap(SelfConsentDTO::getCode, SelfConsentDTO::isAccepted));

    if (initiativeDTO1.getSelfDeclarationList().isEmpty() && list.isEmpty()) {
      return flagList;
    }
    if (list.size() != initiativeDTO.getSelfDeclarationList().size()) {
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(), "The amount of self declaration lists mismatch the amount of flags");
    }

    for (SelfDeclarationDTO consentDTO : initiativeDTO.getSelfDeclarationList()) {
      Boolean flag = list.get(consentDTO.getCode());
      if (flag != null && flag) {
        flagList.add(true);
      } else {
        throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
            String.format(
                "The selfDeclarationList was denied by the user for the initiative %s.",
                consentPutDTO.getInitiativeId()));
      }
    }
    return flagList;
  }
}
