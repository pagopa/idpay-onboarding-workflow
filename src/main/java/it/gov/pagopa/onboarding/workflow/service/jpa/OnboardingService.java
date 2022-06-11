package it.gov.pagopa.onboarding.workflow.service.jpa;

import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.http.ResponseEntity;

public interface OnboardingService {

  Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId);

  OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId);

  ResponseEntity<?> putTcConsent(String initiativeId, String userId);

  void setOnEvaluation(Onboarding onboarding);

  boolean checkPrerequisites(String initiativeId);

  boolean checkCFWhitelist(String initiativeId, String userId);

  RequiredCriteriaDTO getCriteriaLists(String initiativeId);
}
