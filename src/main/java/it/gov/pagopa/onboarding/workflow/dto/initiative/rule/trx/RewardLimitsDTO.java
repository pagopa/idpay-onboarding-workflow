package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.trx;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class RewardLimitsDTO {

  private String frequency;

  private BigDecimal rewardLimit;

}
