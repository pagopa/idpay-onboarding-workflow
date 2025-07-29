package it.gov.pagopa.onboarding.workflow.dto.web;

import com.fasterxml.jackson.annotation.JsonProperty;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeBeneficiaryRuleDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeGeneralDTO;
import lombok.Builder;

import java.time.LocalDateTime;

/**
 * InitiativeWebDTO
 */
@Builder
public class InitiativeWebDTO {


  @JsonProperty("general")
  private InitiativeGeneralDTO general;

  @JsonProperty("beneficiaryRule")
  private InitiativeBeneficiaryRuleDTO beneficiaryRule;

}