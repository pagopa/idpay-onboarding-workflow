package it.gov.pagopa.onboarding.workflow.service.web;


import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;


public interface OnboardingServiceWeb {

  InitiativeWebDTO getInitiativeWeb(String initiativeId);


}
