package it.gov.pagopa.onboarding.workflow.exception.custom.badrequest;

import it.gov.pagopa.common.web.exception.ServiceException;

public class OperationNotAllowedException extends ServiceException {

    public OperationNotAllowedException(String code, String message) {
        this(code, message, false, null);
    }

    public OperationNotAllowedException(String code, String message, boolean printStackTrace, Throwable ex) {
        super(code, message, printStackTrace, ex);
    }
}
