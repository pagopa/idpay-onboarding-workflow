package it.gov.pagopa.onboarding.workflow.dto.initiative.rule.trx;

import lombok.Data;

import java.util.Set;

@Data
public class MccFilterDTO {

  private boolean allowedList;
  private Set<String> values;
}
