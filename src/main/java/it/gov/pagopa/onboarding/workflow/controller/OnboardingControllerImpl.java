package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.ErrorDto;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.service.jpa.OnboardingService;
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

  public ResponseEntity<?> checkPrerequisites(@RequestBody OnboardingPutDTO body,
      @PathVariable("userId") String userId) {
    String initiativeId = body.getInitiativeId();
    Onboarding onboarding = onboardingService.findByInitiativeIdAndUserId(
        initiativeId, userId);

    if (onboarding.isTc() && onboarding.getStatus()
        .equals(OnboardingWorkflowConstants.ACCEPTED_TC)) {
      if (onboardingService.checkPrerequisites(initiativeId)) {
        if (onboardingService.checkCFWhitelist(initiativeId, userId)) {
          onboardingService.setOnEvaluation(onboarding);
          return new ResponseEntity<>(HttpStatus.ACCEPTED);
        } else {
          RequiredCriteriaDTO dto = onboardingService.getCriteriaLists(initiativeId);
          return new ResponseEntity<>(dto,
              HttpStatus.OK);
        }
      } else {
        return new ResponseEntity<>(new ErrorDto(403, "Controlli preliminari falliti"),
            HttpStatus.FORBIDDEN);
      }
    } else {
      return new ResponseEntity<>(new ErrorDto(404, "T&C non ancora accettati"),
          HttpStatus.NOT_FOUND);
    }
  }

  public ResponseEntity<?> consentOnboarding(@RequestBody ConsentPutDTO body,
      @PathVariable("userId") String userId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

  public ResponseEntity<?> onboardingCitizen(@RequestBody OnboardingPutDTO onBoardingPutDTO,
      @PathVariable("userId") String userId) {
    if (onboardingService.putTcConsent(onBoardingPutDTO.getInitiativeId(), userId) == null) {
      return new ResponseEntity<>(new ErrorDto(404, "Iniziativa inesistente"),
          HttpStatus.NOT_FOUND);
    }
    return new ResponseEntity(HttpStatus.NO_CONTENT);
  }

  public ResponseEntity<?> onboardingStatus(
      @PathVariable("initiativeId") String initiativeId, @PathVariable("userId") String userId) {
    OnboardingStatusDTO onBoardingStatusDTO = onboardingService.getOnboardingStatus(initiativeId,
        userId);
    if (onBoardingStatusDTO == null) {
      return new ResponseEntity<>(new ErrorDto(404, "Onboarding inesistente"),
          HttpStatus.NOT_FOUND);
    }
    return new ResponseEntity(onBoardingStatusDTO, HttpStatus.OK);
  }
}
