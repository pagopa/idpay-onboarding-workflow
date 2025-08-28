package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.onboarding.workflow.dto.*;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.enums.ChannelType;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDateTime;
import java.util.Locale;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.INITIATIVE_NOT_FOUND_MSG;

@RestController
public class OnboardingControllerImpl implements OnboardingController {

  private final OnboardingService onboardingService;

  public OnboardingControllerImpl(OnboardingService onboardingService) {
    this.onboardingService = onboardingService;
  }

  @Override
  public ResponseEntity<InitiativeWebDTO> getInitiativeDetail(
          String initiativeId, Locale acceptLanguage) {
    InitiativeWebDTO dto = onboardingService.initiativeDetail(initiativeId, acceptLanguage);
    if (dto == null) {
      throw new InitiativeNotFoundException(String.format(INITIATIVE_NOT_FOUND_MSG, initiativeId), true, null);

    }
    return ResponseEntity.ok(dto);
  }

  public ResponseEntity<OnboardingStatusDTO> onboardingStatus(
          String initiativeId, String userId) {
    OnboardingStatusDTO onBoardingStatusDTO = onboardingService.getOnboardingStatus(initiativeId,
            userId);
    return new ResponseEntity<>(onBoardingStatusDTO, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<Void> saveOnboarding(ConsentPutDTO consentPutDTO, ChannelType channel, String userId) {
    consentPutDTO.setChannel(channel);
    onboardingService.saveOnboarding(consentPutDTO, userId);
    return ResponseEntity.accepted().build();
  }

  @Override
  public ResponseEntity<ResponseInitiativeOnboardingDTO> onboardingStatusList(String initiativeId,
                                                                              Pageable pageable,
                                                                              String userId,
                                                                              LocalDateTime startDate,
                                                                              LocalDateTime endDate,
                                                                              String status) {
    ResponseInitiativeOnboardingDTO responseInitiativeOnboardingDTO = onboardingService.getOnboardingStatusList(
            initiativeId, userId, startDate, endDate,
            status, pageable);
    return new ResponseEntity<>(responseInitiativeOnboardingDTO, HttpStatus.OK);
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

  @Override
  public ResponseEntity<Void> suspend(String initiativeId, String userId) {
    onboardingService.suspend(initiativeId, userId);
    return new ResponseEntity<>(HttpStatus.NO_CONTENT);
  }

  @Override
  public ResponseEntity<Void> readmit(String initiativeId, String userId) {
    onboardingService.readmit(initiativeId,userId);
    return new ResponseEntity<>(HttpStatus.NO_CONTENT);
  }

  @Override
  public  ResponseEntity<OnboardingFamilyDTO> familyUnitComposition(String initiativeId, String userId){
    OnboardingFamilyDTO onboardingFamilyDTO = onboardingService.getfamilyUnitComposition(initiativeId, userId);
    return new ResponseEntity<>(onboardingFamilyDTO, HttpStatus.OK);
  }
  
}
