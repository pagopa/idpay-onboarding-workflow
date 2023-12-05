package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;


public class InitiativeOnboardingException extends ServiceException {

    public InitiativeOnboardingException(String code, String message) {
        this(code, message, false, null);
    }

    public InitiativeOnboardingException(String code, String message, boolean printStackTrace, Throwable ex) {
        super(code, message, printStackTrace, ex);
    }
}
