package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.refund;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class AccumulatedAmountDTO {

  @JsonProperty("accumulatedType")
  private String accumulatedType;

  @JsonProperty("refundThresholdCents")
  private Long refundThresholdCents;
}
