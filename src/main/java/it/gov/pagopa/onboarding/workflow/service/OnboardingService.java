package it.gov.pagopa.onboarding.workflow.service;

import it.gov.pagopa.onboarding.workflow.dto.*;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.model.SelfDeclaration;
import org.springframework.data.domain.Pageable;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Locale;
import java.util.Map;

public interface OnboardingService {

  OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId);
  void putTcConsent(String initiativeId, String userId);
  RequiredCriteriaDTO checkPrerequisites(String initiativeId, String userId, String channel);
  void saveOnboarding(ConsentPutDTO consentPutDTO, String userId);
  InitiativeWebDTO initiativeDetail(String initiativeId, Locale acceptLanguage);
  void completeOnboarding(EvaluationDTO evaluationDTO);
  void allowedInitiative(OnboardingNotificationDTO onboardingNotificationDTO);
  void deactivateOnboarding(String initiativeId, String userId, String deactivationDate);
  void rollback(String initiativeId, String userId);
  ResponseInitiativeOnboardingDTO getOnboardingStatusList(String initiativeId, String userId, LocalDateTime startDate, LocalDateTime endDate, String status, Pageable pageable);
  void suspend(String initiativeId, String userId);
  void readmit(String initiativeId, String userId);
  OnboardingFamilyDTO getfamilyUnitComposition(String initiativeId, String userId);
  void processCommand(QueueCommandOperationDTO queueCommandOperationDTO);
  Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId);
  Onboarding findOnboardingByInitiativeIdAndUserId(String initiativeId, String userId);
  InitiativeDTO getInitiative(String initiativeId);
  void checkStatus(Onboarding onboarding);
  void checkDates(InitiativeDTO initiativeDTO, Onboarding onboarding);
  LocalDate getEndDate(InitiativeDTO initiativeDTO, Onboarding onboarding);
  void checkBudget(InitiativeDTO initiativeDTO, Onboarding onboarding);
  void performanceLog(long startTime, String service, String userId, String initiativeId);
  void selfDeclaration(InitiativeDTO initiativeDTO, ConsentPutDTO consentPutDTO, String userId);
  boolean sizeCheck(InitiativeDTO initiativeDTO, Map<String, Boolean> selfDeclarationBool, Map<String, String> selfDeclarationMulti, Map<String, String> selfDeclarationText);
  void multiCriteriaCheck(InitiativeDTO initiativeDTO, SelfCriteriaMultiDTO multi, Map<String, String> selfDeclarationMulti);
  SelfDeclaration getOrCreateSelfDeclaration(String initiativeId, String userId);
  void handleExistingOnboarding(Onboarding onboarding);
  void validateInput(ConsentPutDTO dto);
  boolean hasAutomatedCriteriaAndPdndNotAccepted(InitiativeDTO initiativeDTO, ConsentPutDTO dto);
  void handlePdndDenied(Onboarding onboarding, String userId, InitiativeDTO initiativeDTO, long startTime);
  void fillOnboardingData(Onboarding onboarding, ConsentPutDTO dto);

}
