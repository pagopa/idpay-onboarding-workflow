package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.common.web.exception.ServiceExceptionPayload;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.BUDGET_EXHAUSTED;

public class InitiativeBudgetExhaustedException extends ServiceException {

    public InitiativeBudgetExhaustedException(String message) {
        this(BUDGET_EXHAUSTED, message);
    }

    public InitiativeBudgetExhaustedException(String code, String message) {
        this(code, message, null, false, null);
    }

    public InitiativeBudgetExhaustedException(String code, String message, ServiceExceptionPayload response, boolean printStackTrace, Throwable ex) {
        super(code, message, response, printStackTrace, ex);
    }
}
