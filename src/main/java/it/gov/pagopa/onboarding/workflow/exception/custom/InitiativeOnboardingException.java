package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;


public class InitiativeOnboardingException extends ServiceException {

    public InitiativeOnboardingException(String code, String message) {
        this(code, message, null, false, null);
    }

    public InitiativeOnboardingException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
