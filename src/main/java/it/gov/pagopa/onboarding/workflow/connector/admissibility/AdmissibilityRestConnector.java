package it.gov.pagopa.onboarding.workflow.connector.admissibility;

import it.gov.pagopa.onboarding.workflow.dto.admissibility.InitiativeStatusDTO;
import org.springframework.web.bind.annotation.PathVariable;

public interface AdmissibilityRestConnector {
  InitiativeStatusDTO getInitiativeStatus(@PathVariable("initiativeId") String initiativeId);
}
