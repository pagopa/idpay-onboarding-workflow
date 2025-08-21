package it.gov.pagopa.onboarding.workflow.service;

import it.gov.pagopa.onboarding.workflow.dto.*;
import org.springframework.data.domain.Pageable;

import java.time.LocalDateTime;

public interface OnboardingService {

  OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId);

  void putTcConsent(String initiativeId, String userId);

  RequiredCriteriaDTO checkPrerequisites(String initiativeId, String userId, String channel);

  void saveConsent(ConsentPutDTO consentPutDTO, String userId);

  void completeOnboarding(EvaluationDTO evaluationDTO);
  
  void allowedInitiative(OnboardingNotificationDTO onboardingNotificationDTO);

  void deactivateOnboarding(String initiativeId, String userId, String deactivationDate);

  void rollback(String initiativeId, String userId);

  ResponseInitiativeOnboardingDTO getOnboardingStatusList(String initiativeId, String userId, LocalDateTime startDate, LocalDateTime endDate, String status, Pageable pageable);

  void suspend(String initiativeId, String userId);
  void readmit(String initiativeId, String userId);
  OnboardingFamilyDTO getfamilyUnitComposition(String initiativeId, String userId);

  void processCommand(QueueCommandOperationDTO queueCommandOperationDTO);

}
