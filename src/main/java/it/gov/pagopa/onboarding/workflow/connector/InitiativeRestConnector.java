package it.gov.pagopa.onboarding.workflow.connector;

import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeIssuerDTO;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.web.bind.annotation.PathVariable;

import java.util.List;

public interface InitiativeRestConnector {
  @Cacheable(value = "initiativeBeneficiaryView", key = "#initiativeId")
  InitiativeDTO getInitiativeBeneficiaryView(@PathVariable("initiativeId") String initiativeId);

  List<InitiativeIssuerDTO> getInitiativeIssuerList();
}
