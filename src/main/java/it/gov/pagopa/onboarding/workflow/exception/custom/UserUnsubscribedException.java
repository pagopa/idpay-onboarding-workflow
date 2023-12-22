package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.USER_UNSUBSCRIBED;

public class UserUnsubscribedException extends ServiceException {

    public UserUnsubscribedException(String message) {
        this(USER_UNSUBSCRIBED, message);
    }

    public UserUnsubscribedException(String code, String message) {
        this(code, message, null, false, null);
    }

    public UserUnsubscribedException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
