package it.gov.pagopa.onboarding.workflow.service;

import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import java.util.Map;

public interface OnboardingService {

  Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId);

  OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId);

  void putTcConsent(String initiativeId, String userId);

  void setStatus(Onboarding onboarding, String status);

  void checkPrerequisites(String initiativeId);

  boolean checkCFWhitelist(String initiativeId, String userId);

  RequiredCriteriaDTO getCriteriaLists(String initiativeId);

  void checkTCStatus(Onboarding onboarding);

  void saveConsent(ConsentPutDTO consentPutDTO, String userId);

  Map<String, Boolean> selfDeclaration(ConsentPutDTO consentPutDTO);

  void completeOnboarding(EvaluationDTO evaluationDTO);

  void deactivateOnboarding(String initiativeId, String userId, String deactivationDate);
}
