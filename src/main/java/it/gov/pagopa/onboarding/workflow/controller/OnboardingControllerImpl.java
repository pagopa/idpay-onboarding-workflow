package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import it.gov.pagopa.onboarding.workflow.service.jpa.OnboardingService;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
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

  @Autowired
  OnboardingRepository onboardingRepository;

  public ResponseEntity<RequiredCriteriaDTO> checkPrerequisites(@RequestBody OnboardingPutDTO body,
      @PathVariable("userId") String userId) {
    Onboarding onboarding = new Onboarding(body.getInitiativeId(), userId);
    onboarding.setStatus("TEST_STATUS");
    onboarding.setCriteriaConsensusTimestamp(new Date());
    onboarding.setTcAcceptTimestamp(new Date());
    List<Boolean> testList = new ArrayList<>();
    testList.add(true);
    testList.add(true);
    testList.add(true);
    onboarding.setSelfDeclarationList(testList);
    onboardingRepository.save(onboarding);
    return new ResponseEntity<RequiredCriteriaDTO>(HttpStatus.OK);
  }

  public ResponseEntity<Void> consentOnboarding(@RequestBody ConsentPutDTO body,
      @PathVariable("userId") String userId) {
    return new ResponseEntity<Void>(HttpStatus.NOT_IMPLEMENTED);
  }

  public ResponseEntity<Void> onboardingCitizen(@RequestBody OnboardingPutDTO onBoardingPutDTO, @PathVariable("userId") String userId) {
    if (onboardingService.putTcConsent(onBoardingPutDTO.getInitiativeId(), userId) == null){
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    }
    return new ResponseEntity(HttpStatus.NO_CONTENT);
  }

  public ResponseEntity<OnboardingStatusDTO> onboardingStatus(
      @PathVariable("initiativeId") String initiativeId, @PathVariable("userId") String userId) {
    OnboardingStatusDTO onBoardingStatusDTO = onboardingService.getOnboardingStatus(initiativeId,
        userId);
    if (onBoardingStatusDTO == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    }
    System.out.println(onBoardingStatusDTO);
    return new ResponseEntity(onBoardingStatusDTO, HttpStatus.OK);

  }

}
