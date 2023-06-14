package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@Data
@NoArgsConstructor
public class UnsubscribeBodyDTO {

  private String initiativeId;
  private String userId;
  private String unsubscribeDate;
}
