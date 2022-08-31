package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.UnsubscribeBodyDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import javax.validation.Valid;
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

  public ResponseEntity<RequiredCriteriaDTO> checkPrerequisites(
      @Valid @RequestBody OnboardingPutDTO body,
      @PathVariable("userId") String userId) {
    String initiativeId = body.getInitiativeId();
    Onboarding onboarding;
    onboarding = onboardingService.findByInitiativeIdAndUserId(
        initiativeId, userId);

    onboardingService.checkTCStatus(onboarding);
    onboardingService.checkPrerequisites(initiativeId);

    if (onboardingService.checkCFWhitelist(initiativeId, userId)) {
      onboardingService.setStatus(onboarding, OnboardingWorkflowConstants.ON_EVALUATION);
      return new ResponseEntity<>(HttpStatus.ACCEPTED);
    } else {
      RequiredCriteriaDTO dto = onboardingService.getCriteriaLists(initiativeId);
      return new ResponseEntity<>(dto,
          HttpStatus.OK);
    }
  }

  public ResponseEntity<Void> onboardingCitizen(
      @Valid @RequestBody OnboardingPutDTO onBoardingPutDTO,
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

  @Override
  public ResponseEntity<Void> saveConsent(@Valid @RequestBody ConsentPutDTO body,
      @PathVariable("userId") String userId) {
    Onboarding onboarding = onboardingService.findByInitiativeIdAndUserId(body.getInitiativeId(),
        userId);
    onboardingService.checkTCStatus(onboarding);
    onboardingService.saveConsent(body, userId);
    return new ResponseEntity<>(HttpStatus.ACCEPTED);
  }

  @Override
  public ResponseEntity<Void> disableOnboarding(UnsubscribeBodyDTO body) {
    onboardingService.deactivateOnboarding(body.getInitiativeId(), body.getUserId(), body.getUnsubscribeDate());
    return new ResponseEntity<>(HttpStatus.NO_CONTENT);
  }
}
