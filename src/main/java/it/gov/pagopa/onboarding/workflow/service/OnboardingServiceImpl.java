package it.gov.pagopa.onboarding.workflow.service;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.PDNDCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfDeclarationDTO;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

@Service
public class OnboardingServiceImpl implements OnboardingService {

  @Autowired
  private OnboardingRepository onboardingRepository;

  @Override
  public Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId) {
    return onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
        .orElseThrow(() -> new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
            String.format("Onboarding with initiativeId %s and userId %s not found.", initiativeId,
                userId)));
  }

  @Override
  public void putTcConsent(String initiativeId, String userId) {
    getInitiative(initiativeId);
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId)
        .orElse(null);
    if (onboarding == null) {
      Onboarding newOnboarding = new Onboarding();
      newOnboarding.setUserId(userId);
      newOnboarding.setInitiativeId(initiativeId);
      newOnboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
      newOnboarding.setTcAcceptTimestamp(Date.from(Instant.now()));
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
  public void checkPrerequisites(
      String initiativeId) { // Integrare con il sottosistema iniziativa

    boolean check = initiativeId.equals("123");
    if (!check) {
      throw new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
          String.format("The initiative with id %s has not met the prerequisites.", initiativeId));
    }
  }

  @Override
  public boolean checkCFWhitelist(String initiativeId,
      String userId) { // Integrare con il sottosistema iniziativa
    return initiativeId.equals("123") && userId.equals("123");
  }

  @Override
  public RequiredCriteriaDTO getCriteriaLists(
      String initiativeId) { // Integrare con il sottosistema iniziativa
    List<PDNDCriteriaDTO> pdndCriteria = new ArrayList<>();
    List<SelfDeclarationDTO> selfDeclarationList = new ArrayList<>();
    return new RequiredCriteriaDTO(pdndCriteria, selfDeclarationList);
  }

  @Override
  public void checkTCStatus(Onboarding onboarding) {
    if (!onboarding.isTc() && !onboarding.getStatus()
        .equals(OnboardingWorkflowConstants.ACCEPTED_TC)) {
      throw new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
          String.format("Terms and Conditions have been not accepted by user %s for initiative %s.",
              onboarding.getUserId(), onboarding.getInitiativeId()));
    }
  }

  private void getInitiative(
      String initiativeId) { // Integrare con il sottosistema iniziativa
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
            String.format("Onboarding with initiativeId %s and userId %s not found.", initiativeId,
                userId)));
    return new OnboardingStatusDTO(onboarding.getStatus());
  }

}
