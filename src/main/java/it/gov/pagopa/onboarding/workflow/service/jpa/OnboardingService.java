package it.gov.pagopa.onboarding.workflow.service.jpa;

import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import org.springframework.http.ResponseEntity;

public interface OnboardingService {

  OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId);
  ResponseEntity<Void> putTcConsent(String initiativeId, String userId);

}
