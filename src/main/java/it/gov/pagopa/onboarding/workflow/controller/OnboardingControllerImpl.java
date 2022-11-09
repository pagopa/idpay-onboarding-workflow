package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.ResponseInitiativeOnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.UnsubscribeBodyDTO;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import java.time.LocalDateTime;
import java.util.logging.Logger;
import javax.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
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
    RequiredCriteriaDTO dto = onboardingService.checkPrerequisites(body.getInitiativeId(), userId);
    if (dto == null) {
      return ResponseEntity.accepted().build();
    }
    return ResponseEntity.ok(dto);
  }

  public ResponseEntity<Void> onboardingCitizen(
      @Valid @RequestBody OnboardingPutDTO onBoardingPutDTO,
      @PathVariable("userId") String userId) {
    Logger logger = Logger.getLogger("AUDIT");
    logger.info("CEF:0|Kemp|LM|1.0|5|Connection failed|3|vs=172.16.151.21:80 event=Connection failed srcip=192.168.10.67 srcport=17548 dstip=172.16.128.37 dstport=82");
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
  public ResponseInitiativeOnboardingDTO onboardingStatusList(String initiativeId,
      Pageable pageable,
      String userId,
      LocalDateTime startDate,
      LocalDateTime endDate,
      String status) {
    return onboardingService.getOnboardingStatusList(initiativeId, userId, startDate, endDate,
        status, pageable);
  }

  @Override
  public ResponseEntity<Void> saveConsent(@Valid @RequestBody ConsentPutDTO body,
      @PathVariable("userId") String userId) {
    onboardingService.saveConsent(body, userId);
    return new ResponseEntity<>(HttpStatus.ACCEPTED);
  }

  @Override
  public ResponseEntity<Void> disableOnboarding(UnsubscribeBodyDTO body) {
    onboardingService.deactivateOnboarding(body.getInitiativeId(), body.getUserId(),
        body.getUnsubscribeDate());
    return new ResponseEntity<>(HttpStatus.NO_CONTENT);
  }

  @Override
  public ResponseEntity<Void> rollback(String initiativeId, String userId) {
    onboardingService.rollback(initiativeId, userId);
    return new ResponseEntity<>(HttpStatus.NO_CONTENT);
  }
}
