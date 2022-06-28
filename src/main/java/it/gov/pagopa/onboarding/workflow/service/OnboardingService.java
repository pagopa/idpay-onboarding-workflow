package it.gov.pagopa.onboarding.workflow.service;

import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import java.util.List;

public interface OnboardingService {

  Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId);

  OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId);

  void putTcConsent(String initiativeId, String userId);

  void setOnEvaluation(Onboarding onboarding);

  void checkPrerequisites(String initiativeId);

  boolean checkCFWhitelist(String initiativeId, String userId);

  RequiredCriteriaDTO getCriteriaLists(String initiativeId);

  void checkTCStatus(Onboarding onboarding);

  void saveConsent(ConsentPutDTO consentPutDTO, String userId);

  List<Boolean> selfDeclaration(ConsentPutDTO consentPutDTO);
}
