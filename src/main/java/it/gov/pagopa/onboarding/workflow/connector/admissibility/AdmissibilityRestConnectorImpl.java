package it.gov.pagopa.onboarding.workflow.connector.admissibility;

import feign.FeignException;
import it.gov.pagopa.onboarding.workflow.dto.admissibility.InitiativeStatusDTO;
import it.gov.pagopa.onboarding.workflow.exception.custom.AdmissibilityInvocationException;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.ERROR_ADMISSIBILITY_INVOCATION_MSG;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.INITIATIVE_NOT_FOUND_MSG;

@Service
@Slf4j
public class AdmissibilityRestConnectorImpl implements AdmissibilityRestConnector{

  private final AdmissibilityRestClient admissibilityRestClient;

  public AdmissibilityRestConnectorImpl(AdmissibilityRestClient admissibilityRestClient) {
    this.admissibilityRestClient = admissibilityRestClient;
  }

  @Override
  public InitiativeStatusDTO getInitiativeStatus(String initiativeId) {
    InitiativeStatusDTO initiativeStatusDTO;
    try {
      initiativeStatusDTO = admissibilityRestClient.getInitiativeStatus(initiativeId);
    } catch (FeignException e){
      if (e.status() == 404){
        log.info("[GET_INITIATIVE_STATUS] Initiative {} was not found.", initiativeId);
        throw new InitiativeNotFoundException(String.format(INITIATIVE_NOT_FOUND_MSG, initiativeId), true, e);
      }

      log.error("[GET_INITIATIVE_STATUS] Initiative {}: something went wrong when invoking the API.", initiativeId);
      throw new AdmissibilityInvocationException(ERROR_ADMISSIBILITY_INVOCATION_MSG, true, e);
    }

    return initiativeStatusDTO;
  }
}
