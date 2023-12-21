package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.PDND_CONSENT_DENIED;

public class PDNDConsentDeniedException extends ServiceException {

    public PDNDConsentDeniedException(String message) {
        this(PDND_CONSENT_DENIED, message);
    }

    public PDNDConsentDeniedException(String code, String message) {
        this(code, message, null, false, null);
    }

    public PDNDConsentDeniedException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
