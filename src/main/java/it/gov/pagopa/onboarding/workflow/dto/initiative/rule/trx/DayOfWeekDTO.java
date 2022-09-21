package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.trx;

import java.time.DayOfWeek;
import java.time.LocalTime;
import java.util.List;
import java.util.Set;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;

@Data
public class DayOfWeekDTO {

  private Set<DayOfWeek> daysOfWeek;
  private List<Interval> intervals;

  @Getter
  @AllArgsConstructor
  public static class Interval {

    private LocalTime startTime;
    private LocalTime endTime;
  }
}
