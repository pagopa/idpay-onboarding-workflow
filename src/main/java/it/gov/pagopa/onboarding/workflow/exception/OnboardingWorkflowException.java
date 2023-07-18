package it.gov.pagopa.onboarding.workflow.exception;

import it.gov.pagopa.common.web.exception.ClientExceptionWithBody;
import lombok.Getter;
import org.springframework.http.HttpStatus;

@Getter
@SuppressWarnings("squid:S110")
public class OnboardingWorkflowException extends ClientExceptionWithBody {

  public OnboardingWorkflowException(int code, String message, String details) {
    super(HttpStatus.valueOf(code), String.valueOf(code), message, details);
  }
}
