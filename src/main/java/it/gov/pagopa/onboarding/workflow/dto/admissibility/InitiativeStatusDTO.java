package it.gov.pagopa.onboarding.workflow.dto.admissibility;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class InitiativeStatusDTO {
  private String status;
  private boolean budgetAvailable;
}
