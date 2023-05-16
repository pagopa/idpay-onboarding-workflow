package it.gov.pagopa.onboarding.workflow.connector.admissibility;

import it.gov.pagopa.onboarding.workflow.dto.admissibility.InitiativeStatusDTO;
import org.springframework.stereotype.Service;

@Service
public class AdmissibilityRestConnectorImpl implements AdmissibilityRestConnector{

  private final AdmissibilityRestClient admissibilityRestClient;

  public AdmissibilityRestConnectorImpl(AdmissibilityRestClient admissibilityRestClient) {
    this.admissibilityRestClient = admissibilityRestClient;
  }

  @Override
  public InitiativeStatusDTO getInitiativeStatus(String initiativeId) {
    return admissibilityRestClient.getInitiativeStatus(initiativeId);
  }
}
