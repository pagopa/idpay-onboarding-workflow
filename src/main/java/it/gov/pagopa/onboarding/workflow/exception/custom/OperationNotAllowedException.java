package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

public class OperationNotAllowedException extends ServiceException {

    public OperationNotAllowedException(String code, String message) {
        this(code, message, null, false, null);
    }

    public OperationNotAllowedException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
