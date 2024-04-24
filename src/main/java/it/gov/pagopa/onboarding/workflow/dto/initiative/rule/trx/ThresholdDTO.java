package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.trx;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class ThresholdDTO {
  private BigDecimal from;

  private Boolean fromIncluded;

  private BigDecimal to;

  private Boolean toIncluded;

}
