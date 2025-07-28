package it.gov.pagopa.onboarding.workflow.controller.web;

import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.service.web.OnboardingServiceWeb;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class OnboardingControllerWebImpl implements OnboardingControllerWeb {

  private final OnboardingServiceWeb onboardingServiceWeb;

  public OnboardingControllerWebImpl(OnboardingServiceWeb onboardingServiceWeb) {
    this.onboardingServiceWeb = onboardingServiceWeb;
  }


  @Override
  public ResponseEntity<InitiativeWebDTO> getInitiativeWeb(
          @PathVariable("initiativeId") String initiativeId) {
    InitiativeWebDTO dto = onboardingServiceWeb.getInitiativeWeb(initiativeId);
    if (dto == null) {
      return ResponseEntity.accepted().build();
    }
    return ResponseEntity.ok(dto);
  }


  
}
