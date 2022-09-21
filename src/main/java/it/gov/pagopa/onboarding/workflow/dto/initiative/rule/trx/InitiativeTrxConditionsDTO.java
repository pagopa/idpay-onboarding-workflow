package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.trx;

import java.util.List;
import lombok.Data;

@Data
public class InitiativeTrxConditionsDTO {

  private DayOfWeekDTO daysOfWeek;

  private ThresholdDTO threshold;

  private TrxCountDTO trxCount;

  private MccFilterDTO mccFilter;

  private List<RewardLimitsDTO> rewardLimits;
}