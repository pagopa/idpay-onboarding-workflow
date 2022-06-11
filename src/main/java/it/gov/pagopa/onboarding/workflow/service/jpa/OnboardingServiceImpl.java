package it.gov.pagopa.onboarding.workflow.service.jpa;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.PDNDCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfDeclarationDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public class OnboardingServiceImpl implements OnboardingService {

  @Autowired
  private OnboardingRepository onboardingRepository;

  @Override
  public Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId) {
    return onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId);
  }

  @Override
  public OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId) {
    OnboardingStatusDTO onboardingStatusDTO = null;
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId);
    if (onboarding != null) {
      onboardingStatusDTO = new OnboardingStatusDTO(onboarding.getStatus());
    }
    //System.out.println(onboardingStatusDTO);
    return onboardingStatusDTO;
  }

  @Override
  public ResponseEntity<?> putTcConsent(String initiativeId, String userId) {
    if (this.checkIniziativa(initiativeId) == false) {
      return null;
    }
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId);
    if (onboarding == null) {
      Onboarding newOnboarding = new Onboarding();
      newOnboarding.setUserId(userId);
      newOnboarding.setInitiativeId(initiativeId);
      newOnboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
      newOnboarding.setTcAcceptTimestamp(new Date());
      newOnboarding.setTc(true);
      onboardingRepository.save(newOnboarding);


    }
    return new ResponseEntity<>(HttpStatus.NO_CONTENT);

  }

  @Override
  public void setOnEvaluation(Onboarding onboarding) {
    onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
    onboardingRepository.save(onboarding);
  }

  @Override
  public boolean checkPrerequisites(
      String initiativeId) { //TODO Integrare con il sottosistema iniziativa
    return Math.random() < 0.5;
  }

  @Override
  public boolean checkCFWhitelist(String initiativeId,
      String userId) { //TODO Integrare con il sottosistema iniziativa
    return Math.random() < 0.5;
  }

  @Override
  public RequiredCriteriaDTO getCriteriaLists(
      String initiativeId) { //TODO Integrare con il sottosistema iniziativa
    List<PDNDCriteriaDTO> pdndCriteria = new ArrayList<>();
    List<SelfDeclarationDTO> selfDeclarationList = new ArrayList<>();
    return new RequiredCriteriaDTO(pdndCriteria, selfDeclarationList);
  }

  public boolean checkIniziativa(String initiativeId) {//controllo mock
    return Math.random() < 0.5;
  }
}
