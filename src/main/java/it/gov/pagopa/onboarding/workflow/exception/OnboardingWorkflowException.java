package it.gov.pagopa.onboarding.workflow.exception;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class OnboardingWorkflowException extends RuntimeException {

  private final int code;

  private final String message;

}
