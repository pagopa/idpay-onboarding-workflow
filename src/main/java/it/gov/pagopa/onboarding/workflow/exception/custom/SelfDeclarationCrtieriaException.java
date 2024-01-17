package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.SELF_DECLARATION_NOT_VALID;

public class SelfDeclarationCrtieriaException extends ServiceException {

    public SelfDeclarationCrtieriaException(String message) {
        this(SELF_DECLARATION_NOT_VALID, message);
    }

    public SelfDeclarationCrtieriaException(String code, String message) {
        this(code, message, null, false, null);
    }

    public SelfDeclarationCrtieriaException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
