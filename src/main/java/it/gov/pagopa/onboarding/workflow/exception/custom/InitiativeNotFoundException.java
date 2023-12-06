package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.INITIATIVE_NOT_FOUND;

public class InitiativeNotFoundException extends ServiceException {

    public InitiativeNotFoundException(String message) {
        this(INITIATIVE_NOT_FOUND, message);
    }

    public InitiativeNotFoundException(String code, String message) {
        this(code, message, false, null);
    }

    public InitiativeNotFoundException(String code, String message, boolean printStackTrace, Throwable ex) {
        super(code, message, printStackTrace, ex);
    }
}
