package it.gov.pagopa.onboarding.workflow.dto;

import javax.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class OnboardingRejectionReason {

  @NotNull
  private String type;
  @NotNull
  private String code;
  private String authority;
  private String authorityLabel;
  private String detail;
}
