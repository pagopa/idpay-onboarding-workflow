package it.gov.pagopa.onboarding.workflow.service.web;


import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;

import java.util.Locale;


public interface OnboardingServiceWeb {

  InitiativeWebDTO getInitiativeWeb(String initiativeId, Locale acceptLanguage);

  void saveConsentWeb(ConsentPutDTO consentPutDTO, String userId);

}
