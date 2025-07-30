package it.gov.pagopa.onboarding.workflow.dto.web;

import com.fasterxml.jackson.annotation.JsonProperty;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeBeneficiaryRuleDTO;
import lombok.Builder;


/**
 * InitiativeWebDTO
 */
@Builder
public class InitiativeWebDTO {


  @JsonProperty("general")
  private InitiativeGeneralWebDTO generalWeb;

  @JsonProperty("beneficiaryRule")
  private InitiativeBeneficiaryRuleDTO beneficiaryRule;


}