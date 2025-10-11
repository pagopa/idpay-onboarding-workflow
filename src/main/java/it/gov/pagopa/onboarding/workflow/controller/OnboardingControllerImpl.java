package it.gov.pagopa.onboarding.workflow.controller;

import com.leakyabstractions.result.api.Result;
import it.gov.pagopa.common.web.dto.ErrorDTO;
import it.gov.pagopa.onboarding.workflow.dto.*;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.util.Locale;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.INITIATIVE_NOT_FOUND_MSG;


@RestController
public class OnboardingControllerImpl implements OnboardingController {

  private final OnboardingService onboardingService;

  public OnboardingControllerImpl(OnboardingService onboardingService) {
    this.onboardingService = onboardingService;
  }

  private static ResponseEntity<Object> mapErrorToResponse(ErrorDTO error) {
    HttpStatus status = switch (error.getCode()) {
      case "USER_NOT_ONBOARDED" -> HttpStatus.NOT_FOUND;
      default -> HttpStatus.INTERNAL_SERVER_ERROR;
    };
    return new ResponseEntity<>(error, status);
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

  @Override
  public ResponseEntity<Object> onboardingStatus(
          String initiativeId, String userId) {
    Result<OnboardingStatusDTO, ErrorDTO> result = onboardingService.getOnboardingStatus(initiativeId, userId);
    return result.mapSuccess(
            onBoardingStatusDTO -> new ResponseEntity<Object>(onBoardingStatusDTO, HttpStatus.OK))
        .getSuccess()
        .orElse(
            result.mapFailure(OnboardingControllerImpl::mapErrorToResponse)
            .getFailure().orElse(ResponseEntity.internalServerError().build()));
  }

  @Override
  public ResponseEntity<Void> saveOnboarding(ConsentPutDTO consentPutDTO, String channel, String userId) {
    onboardingService.saveOnboarding(consentPutDTO, channel, userId);
    return ResponseEntity.accepted().build();
  }

  @Override
  public ResponseEntity<ResponseInitiativeOnboardingDTO> onboardingStatusList(Pageable pageable,
                                                                              String userId,
                                                                              String acceptLanguage) {
    ResponseInitiativeOnboardingDTO responseInitiativeOnboardingDTO = onboardingService.getOnboardingStatusList(
            userId, pageable);
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
