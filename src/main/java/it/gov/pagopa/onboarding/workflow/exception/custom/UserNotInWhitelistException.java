package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.USER_NOT_IN_WHITELIST;

public class UserNotInWhitelistException extends ServiceException {

    public UserNotInWhitelistException(String message) {
        this(USER_NOT_IN_WHITELIST, message);
    }

    public UserNotInWhitelistException(String code, String message) {
        this(code, message, false, null);
    }

    public UserNotInWhitelistException(String code, String message, boolean printStackTrace, Throwable ex) {
        super(code, message, printStackTrace, ex);
    }
}
