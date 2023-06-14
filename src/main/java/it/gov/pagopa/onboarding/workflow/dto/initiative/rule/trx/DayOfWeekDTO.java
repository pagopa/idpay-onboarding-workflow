package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.trx;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.time.DayOfWeek;
import java.time.LocalTime;
import java.util.List;
import java.util.Set;

@Data
public class DayOfWeekDTO {

  private Set<DayOfWeek> daysOfWeek;
  private List<Interval> intervals;

  @Data
  @AllArgsConstructor
  public static class Interval {

    private LocalTime startTime;
    private LocalTime endTime;
  }
}
