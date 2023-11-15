package it.gov.pagopa.onboarding.workflow.exception.custom.forbidden;

import it.gov.pagopa.common.web.exception.ServiceException;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.USER_UNSUBSCRIBED;

public class UserUnsubscribedException extends ServiceException {

    public UserUnsubscribedException(String message) {
        this(USER_UNSUBSCRIBED, message);
    }

    public UserUnsubscribedException(String code, String message) {
        this(code, message, false, null);
    }

    public UserUnsubscribedException(String code, String message, boolean printStackTrace, Throwable ex) {
        super(code, message, printStackTrace, ex);
    }
}
