package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class OnboardingControllerImpl implements OnboardingController {

  @Autowired
  OnboardingService onboardingService;

  public ResponseEntity<RequiredCriteriaDTO> checkPrerequisites(@RequestBody OnboardingPutDTO body,
      @PathVariable("userId") String userId) {
    String initiativeId = body.getInitiativeId();
    Onboarding onboarding;
    onboarding = onboardingService.findByInitiativeIdAndUserId(
        initiativeId, userId);

    onboardingService.checkTCStatus(onboarding);
    onboardingService.checkPrerequisites(initiativeId);

    if (onboardingService.checkCFWhitelist(initiativeId, userId)) {
      onboardingService.setOnEvaluation(onboarding);
      return new ResponseEntity<>(HttpStatus.ACCEPTED);
    } else {
      RequiredCriteriaDTO dto = onboardingService.getCriteriaLists(initiativeId);
      return new ResponseEntity<>(dto,
          HttpStatus.OK);
    }
  }

  public ResponseEntity<Void> consentOnboarding(@RequestBody ConsentPutDTO body,
      @PathVariable("userId") String userId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  public ResponseEntity<Void> onboardingCitizen(@RequestBody OnboardingPutDTO onBoardingPutDTO,
      @PathVariable("userId") String userId) {
    onboardingService.putTcConsent(onBoardingPutDTO.getInitiativeId(), userId);
    return new ResponseEntity<>(HttpStatus.NO_CONTENT);
  }

  public ResponseEntity<OnboardingStatusDTO> onboardingStatus(
      @PathVariable("initiativeId") String initiativeId, @PathVariable("userId") String userId) {
    OnboardingStatusDTO onBoardingStatusDTO = onboardingService.getOnboardingStatus(initiativeId,
        userId);
    return new ResponseEntity<>(onBoardingStatusDTO, HttpStatus.OK);
  }

}
