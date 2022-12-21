package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class OnboardingRejectionReason {

  private String type;
  private String code;
  private String authority;
  private String authorityLabel;
  private String detail;
}
