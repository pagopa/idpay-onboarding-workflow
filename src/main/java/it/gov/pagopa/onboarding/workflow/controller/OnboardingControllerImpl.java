package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.*;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.service.jpa.OnboardingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.List;

@RestController
public class OnboardingControllerImpl implements OnboardingController {

  @Autowired
  OnboardingService onboardingService;

  public ResponseEntity<?> checkPrerequisites(@RequestBody OnboardingPutDTO body,
      @PathVariable("userId") String userId) {

    Onboarding onboarding = onboardingService.findByInitiativeIdAndUserId(
        body.getInitiativeId(), userId);

    if (onboarding.isTc() && onboarding.getStatus()
        .equals(OnboardingWorkflowConstants.ACCEPTED_TC)) {
      boolean controlliOk = Math.random() < 0.5;
      boolean cfWhitelist = Math.random() < 0.5;
      if (controlliOk) {
        if (cfWhitelist) {
          onboardingService.setOnEvaluation(onboarding);
          return new ResponseEntity<>(HttpStatus.ACCEPTED);
        } else {
          List<PDNDCriteriaDTO> pdndCriteria = new ArrayList<>();
          List<SelfDeclarationDTO> selfDeclarationList = new ArrayList<>();
          return new ResponseEntity<>(new RequiredCriteriaDTO(pdndCriteria, selfDeclarationList),
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
      if(onboardingService.putTcConsent(onBoardingPutDTO.getInitiativeId(), userId) == null){
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
      return new ResponseEntity<>(new ErrorDto(404, "Onbording inesistente"),
              HttpStatus.NOT_FOUND);
    }
    System.out.println(onBoardingStatusDTO);
    return new ResponseEntity(onBoardingStatusDTO, HttpStatus.OK);
  }
}
