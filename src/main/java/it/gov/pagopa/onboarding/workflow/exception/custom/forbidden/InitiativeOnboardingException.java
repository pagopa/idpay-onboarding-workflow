package it.gov.pagopa.onboarding.workflow.exception.custom.forbidden;

import it.gov.pagopa.common.web.exception.ServiceException;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.GENERIC_ERROR;

public class InitiativeOnboardingException extends ServiceException {

    public InitiativeOnboardingException(String message) {
        this(GENERIC_ERROR, message);
    }

    public InitiativeOnboardingException(String code, String message) {
        this(code, message, false, null);
    }

    public InitiativeOnboardingException(String code, String message, boolean printStackTrace, Throwable ex) {
        super(code, message, printStackTrace, ex);
    }
}
