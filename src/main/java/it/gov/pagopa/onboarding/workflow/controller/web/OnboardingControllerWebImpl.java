package it.gov.pagopa.onboarding.workflow.controller.web;

import it.gov.pagopa.onboarding.workflow.dto.web.ConsentPutWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import it.gov.pagopa.onboarding.workflow.service.web.OnboardingServiceWeb;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
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
          @PathVariable("initiativeId") String initiativeId,
          @RequestHeader(value = "Accept-Language", defaultValue = "it_IT") Locale acceptLanguage) {
    InitiativeWebDTO dto = onboardingServiceWeb.getInitiativeWeb(initiativeId, acceptLanguage);
    if (dto == null) {
      throw new InitiativeNotFoundException(String.format(INITIATIVE_NOT_FOUND_MSG, initiativeId), true, null);

    }
    return ResponseEntity.ok(dto);
  }


  @Override
  public ResponseEntity<Void> saveConsentWeb(
          @RequestBody @Valid ConsentPutWebDTO consentPutWebDTO,
          @PathVariable("userId") String userId) {
    onboardingServiceWeb.saveConsentWeb(consentPutWebDTO, userId);
    return ResponseEntity.accepted().build();
  }

}
