package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.trx;

import java.math.BigDecimal;
import lombok.Data;

@Data
public class ThresholdDTO {
  private BigDecimal from;

  private Boolean fromIncluded;

  private BigDecimal to;

  private Boolean toIncluded;
}
