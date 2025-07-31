package it.gov.pagopa.onboarding.workflow.dto.web;

import com.fasterxml.jackson.annotation.JsonProperty;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeAdditionalDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeBeneficiaryRuleDTO;
import lombok.Builder;


/**
 * InitiativeWebDTO
 */
@Builder
public class InitiativeWebDTO {


  @JsonProperty("additionalInfo")
  private InitiativeAdditionalDTO additionalInfo;

  @JsonProperty("beneficiaryRule")
  private InitiativeBeneficiaryRuleDTO beneficiaryRule;


}