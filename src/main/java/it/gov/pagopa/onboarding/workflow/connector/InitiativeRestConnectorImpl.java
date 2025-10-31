package it.gov.pagopa.onboarding.workflow.connector;

import feign.FeignException;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeIssuerDTO;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeInvocationException;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.*;

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
        throw new InitiativeNotFoundException(String.format(INITIATIVE_NOT_FOUND_MSG, initiativeId), true, e);
      }

      throw new InitiativeInvocationException(ERROR_INITIATIVE_INVOCATION_MSG, true, e);
    }

    return initiativeDTO;
  }

  @Override
  public List<InitiativeIssuerDTO> getInitiativeIssuerList(){
    List<InitiativeIssuerDTO> initiativeIssuerDTO;
    try{
      initiativeIssuerDTO = initiativeRestClient.getInitiativeIssuerList();
    }catch (FeignException e){
      if (e.status() == 404){
        throw new InitiativeNotFoundException(String.format(INITIATIVES_NOT_FOUND_MSG), true, e);
      }

      throw new InitiativeInvocationException(ERROR_INITIATIVE_INVOCATION_MSG, true, e);
    }
    return initiativeIssuerDTO;
  }
}
