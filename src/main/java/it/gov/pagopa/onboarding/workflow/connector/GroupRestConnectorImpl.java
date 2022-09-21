package it.gov.pagopa.onboarding.workflow.connector;

import it.gov.pagopa.onboarding.workflow.dto.initiative.CitizenStatusDTO;

public class GroupRestConnectorImpl implements
    GroupRestConnector {

  private final GroupRestClient groupRestClient;

  public GroupRestConnectorImpl(GroupRestClient groupRestClient) {
    this.groupRestClient = groupRestClient;
  }

  @Override
  public CitizenStatusDTO getCitizenStatus(String initiativeId, String userId) {
    return groupRestClient.getCitizenStatus(initiativeId, userId);
  }
}
