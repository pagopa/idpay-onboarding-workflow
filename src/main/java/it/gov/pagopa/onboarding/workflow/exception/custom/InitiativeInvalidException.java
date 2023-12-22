package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

public class InitiativeInvalidException extends ServiceException {

    public InitiativeInvalidException(String code, String message) {
        this(code, message, null, false, null);
    }

    public InitiativeInvalidException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
