package it.gov.pagopa.onboarding.workflow.service.web;


import it.gov.pagopa.onboarding.workflow.dto.ConsentPutUnifiedDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;

import java.util.Locale;


public interface OnboardingServiceWeb {

  InitiativeWebDTO getInitiativeWeb(String initiativeId, Locale acceptLanguage);

  void saveConsentUnified(ConsentPutUnifiedDTO consentPutUnifiedDTO, String userId);

  void handleExistingOnboarding(Onboarding onboarding);

  void validateInput(ConsentPutUnifiedDTO dto);

  boolean hasAutomatedCriteriaAndPdndNotAccepted(InitiativeDTO initiativeDTO, ConsentPutUnifiedDTO dto);

  void handlePdndDenied(Onboarding onboarding, String userId, InitiativeDTO initiativeDTO, long startTime);

  void fillOnboardingData(Onboarding onboarding, ConsentPutUnifiedDTO dto);

}
