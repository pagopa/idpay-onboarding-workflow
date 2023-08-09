package it.gov.pagopa.onboarding.workflow.connector.admissibility;

import feign.FeignException;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.admissibility.InitiativeStatusDTO;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

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
    try{
      initiativeStatusDTO = admissibilityRestClient.getInitiativeStatus(initiativeId);
    }catch (FeignException e){
      log.error("[GET_INITIATIVE_STATUS] Initiative {}: something went wrong when invoking the API.",
          initiativeId);
      throw new OnboardingWorkflowException(e.status(), e.contentUTF8(), OnboardingWorkflowConstants.GENERIC_ERROR, e);
    }
    return initiativeStatusDTO;
  }
}
