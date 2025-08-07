package it.gov.pagopa.onboarding.workflow.connector;

import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.web.bind.annotation.PathVariable;

public interface InitiativeRestConnector {
  //@Cacheable(value = "initiativeBeneficiaryView", key = "#initiativeId")
  InitiativeDTO getInitiativeBeneficiaryView(@PathVariable("initiativeId") String initiativeId);
}
