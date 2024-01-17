package it.gov.pagopa.onboarding.workflow.connector;

import feign.FeignException;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeInvocationException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.ERROR_INITIATIVE_INVOCATION_MSG;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.INITIATIVE_NOT_FOUND_MSG;

@Service
@Slf4j
public class InitiativeRestConnectorImpl implements InitiativeRestConnector {

  private final InitiativeRestClient initiativeRestClient;

  public InitiativeRestConnectorImpl(
      InitiativeRestClient initiativeRestClient) {
    this.initiativeRestClient = initiativeRestClient;
  }

  @Override
  public InitiativeDTO getInitiativeBeneficiaryView(String initiativeId) {
    InitiativeDTO initiativeDTO;
    try {
      initiativeDTO = initiativeRestClient.getInitiativeBeneficiaryView(initiativeId);
    } catch (FeignException e){
      if (e.status() == 404){
        log.error("[GET_INITIATIVE_BENEFICIARY_VIEW] Initiative {} was not found.", initiativeId);
        throw new InitiativeNotFoundException(String.format(INITIATIVE_NOT_FOUND_MSG, initiativeId), true, e);
      }

      log.error("[GET_INITIATIVE_BENEFICIARY_VIEW] Initiative {}: something went wrong when invoking the API.", initiativeId);
      throw new InitiativeInvocationException(ERROR_INITIATIVE_INVOCATION_MSG, true, e);
    }

    return initiativeDTO;
  }
}
