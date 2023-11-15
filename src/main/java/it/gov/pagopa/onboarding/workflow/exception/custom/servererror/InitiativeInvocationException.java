package it.gov.pagopa.onboarding.workflow.exception.custom.servererror;

import it.gov.pagopa.common.web.exception.ServiceException;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.GENERIC_ERROR;

public class InitiativeInvocationException extends ServiceException {

    public InitiativeInvocationException(String message) {
        this(GENERIC_ERROR, message);
    }

    public InitiativeInvocationException(String code, String message) {
        this(code, message, false, null);
    }

    public InitiativeInvocationException(String code, String message, boolean printStackTrace, Throwable ex) {
        super(code, message, printStackTrace, ex);
    }
}
