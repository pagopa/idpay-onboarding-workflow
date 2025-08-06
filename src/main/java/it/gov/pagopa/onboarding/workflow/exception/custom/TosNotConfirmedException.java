package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.TOS_NOT_CONFIRMED;

public class TosNotConfirmedException extends ServiceException {
    public TosNotConfirmedException(String message) {
        this(TOS_NOT_CONFIRMED, message, null, false, null);
    }

    public TosNotConfirmedException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
