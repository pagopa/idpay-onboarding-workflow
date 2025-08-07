package it.gov.pagopa.onboarding.workflow.controller.web;

import it.gov.pagopa.onboarding.workflow.dto.ConsentPutUnifiedDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import it.gov.pagopa.onboarding.workflow.service.web.OnboardingServiceWeb;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.util.Locale;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.INITIATIVE_NOT_FOUND_MSG;

@RestController
public class OnboardingControllerWebImpl implements OnboardingControllerWeb {

  private final OnboardingServiceWeb onboardingServiceWeb;

  public OnboardingControllerWebImpl(OnboardingServiceWeb onboardingServiceWeb) {
    this.onboardingServiceWeb = onboardingServiceWeb;
  }


  @Override
  public ResponseEntity<InitiativeWebDTO> getInitiativeWeb(
          String initiativeId, Locale acceptLanguage) {
    InitiativeWebDTO dto = onboardingServiceWeb.getInitiativeWeb(initiativeId, acceptLanguage);
    if (dto == null) {
      throw new InitiativeNotFoundException(String.format(INITIATIVE_NOT_FOUND_MSG, initiativeId), true, null);

    }
    return ResponseEntity.ok(dto);
  }



  @Override
  public ResponseEntity<Void> saveConsentUnified(ConsentPutUnifiedDTO consentPutUnifiedDTO, String channel, String userId) {
    consentPutUnifiedDTO.setChannel(channel);
    onboardingServiceWeb.saveConsentUnified(consentPutUnifiedDTO, userId);
    return ResponseEntity.accepted().build();
  }
}
