package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.PAGE_SIZE_NOT_ALLOWED;

public class PageSizeNotAllowedException extends ServiceException {

    public PageSizeNotAllowedException(String message) {
        this(PAGE_SIZE_NOT_ALLOWED, message);
    }

    public PageSizeNotAllowedException(String code, String message) {
        this(code, message, false, null);
    }

    public PageSizeNotAllowedException(String code, String message, boolean printStackTrace, Throwable ex) {
        super(code, message, printStackTrace, ex);
    }
}
