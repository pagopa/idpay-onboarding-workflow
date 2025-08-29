package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.trx;

import lombok.Data;

import java.util.List;

@Data
public class InitiativeTrxConditionsDTO {

  private DayOfWeekDTO daysOfWeek;

  private ThresholdDTO threshold;

  private TrxCountDTO trxCount;

  private MccFilterDTO mccFilter;

  private List<RewardLimitsDTO> rewardLimits;
}