package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.trx;

import lombok.Data;

@Data
public class RewardLimitsDTO {

  private String frequency;

  private Long rewardLimitCents;
}
