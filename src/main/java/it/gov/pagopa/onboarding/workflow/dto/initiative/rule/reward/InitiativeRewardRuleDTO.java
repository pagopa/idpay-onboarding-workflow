package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.reward;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class InitiativeRewardRuleDTO {

  @JsonProperty("_type")
  private String type;
}
