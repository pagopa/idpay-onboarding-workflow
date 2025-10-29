package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@Data
@NoArgsConstructor
@Builder
public class UnsubscribeBodyDTO {

  private String initiativeId;
  private String userId;
  private String unsubscribeDate;
  @Builder.Default
  private Boolean updateFamilyMembers = true;
}
