package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingStatusCitizenDTO {

  private String initiativeName;
  private String serviceId;
  private String initiativeId;
  private String status;
  private String statusDate;
}

