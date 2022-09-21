package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.trx;

import java.math.BigDecimal;
import lombok.Data;

@Data
public class RewardLimitsDTO {

  private String frequency;

  private BigDecimal rewardLimit;
}
