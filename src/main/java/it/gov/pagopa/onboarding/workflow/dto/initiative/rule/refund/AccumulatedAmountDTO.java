package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.refund;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class AccumulatedAmountDTO {

  @JsonProperty("accumulatedType")
  private String accumulatedType;

  @JsonProperty("refundThreshold")
  private BigDecimal refundThreshold;

}
