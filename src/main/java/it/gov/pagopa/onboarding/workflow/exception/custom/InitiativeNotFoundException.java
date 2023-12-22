package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.INITIATIVE_NOT_FOUND;

public class InitiativeNotFoundException extends ServiceException {

    public InitiativeNotFoundException(String message, boolean printStackTrace, Throwable ex) {
        this(INITIATIVE_NOT_FOUND, message, null, printStackTrace, ex);
    }

    public InitiativeNotFoundException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
