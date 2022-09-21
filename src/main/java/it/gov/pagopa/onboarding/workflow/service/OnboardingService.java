package it.gov.pagopa.onboarding.workflow.service;

import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;

public interface OnboardingService {

  OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId);

  void putTcConsent(String initiativeId, String userId);

  RequiredCriteriaDTO checkPrerequisites(String initiativeId, String userId);

  void saveConsent(ConsentPutDTO consentPutDTO, String userId);

  void completeOnboarding(EvaluationDTO evaluationDTO);

  void deactivateOnboarding(String initiativeId, String userId, String deactivationDate);

  void rollback(String initiativeId, String userId);
}
