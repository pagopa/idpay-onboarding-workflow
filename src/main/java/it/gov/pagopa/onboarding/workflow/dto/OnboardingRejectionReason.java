package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingRejectionReason {

  private String type;
  private String code;
  private String authority;
  private String authorityLabel;
  private String detail;
}
