package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.GENERIC_ERROR;

public class UserSuspensionOrReadmissionException extends ServiceException {

    public UserSuspensionOrReadmissionException(String message) {
        this(GENERIC_ERROR, message);
    }

    public UserSuspensionOrReadmissionException(String code, String message) {
        this(code, message, null, false, null);
    }

    public UserSuspensionOrReadmissionException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
