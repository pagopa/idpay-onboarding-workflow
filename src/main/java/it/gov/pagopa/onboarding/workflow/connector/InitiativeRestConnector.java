package it.gov.pagopa.onboarding.workflow.connector;

import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import org.springframework.web.bind.annotation.PathVariable;

public interface InitiativeRestConnector {
  InitiativeDTO getInitiativeBeneficiaryView(@PathVariable("initiativeId") String initiativeId);
}
