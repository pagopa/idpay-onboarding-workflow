package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.GENERIC_ERROR;

public class PDVInvocationException extends ServiceException {

    public PDVInvocationException(String message, boolean printStackTrace, Throwable ex) {
        this(GENERIC_ERROR, message, null, printStackTrace, ex);
    }

    public PDVInvocationException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
