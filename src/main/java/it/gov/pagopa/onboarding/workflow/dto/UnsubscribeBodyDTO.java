package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class UnsubscribeBodyDTO {

  private String initiativeId;
  private String userId;
  private String unsubscribeDate;
}
