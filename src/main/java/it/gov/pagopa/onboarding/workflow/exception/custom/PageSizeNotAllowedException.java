package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.PAGE_SIZE_NOT_ALLOWED;

public class PageSizeNotAllowedException extends ServiceException {

    public PageSizeNotAllowedException(String message) {
        this(PAGE_SIZE_NOT_ALLOWED, message);
    }

    public PageSizeNotAllowedException(String code, String message) {
        this(code, message, null, false, null);
    }

    public PageSizeNotAllowedException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
