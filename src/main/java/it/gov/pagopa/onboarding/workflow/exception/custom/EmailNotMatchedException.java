package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.EMAIL_NOT_MATCHED;

public class EmailNotMatchedException extends ServiceException {

  public EmailNotMatchedException(String message) {
    this(EMAIL_NOT_MATCHED, message, null, false, null);
  }

  public EmailNotMatchedException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
    super(code, message, response, printStackTrace, ex);
  }
}
