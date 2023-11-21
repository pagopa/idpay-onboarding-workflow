package it.gov.pagopa.onboarding.workflow.exception.custom;

import it.gov.pagopa.common.web.exception.ServiceException;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.BUDGET_EXHAUSTED;

public class InitiativeBudgetExhaustedException extends ServiceException {

    public InitiativeBudgetExhaustedException(String message) {
        this(BUDGET_EXHAUSTED, message);
    }

    public InitiativeBudgetExhaustedException(String code, String message) {
        this(code, message, false, null);
    }

    public InitiativeBudgetExhaustedException(String code, String message, boolean printStackTrace, Throwable ex) {
        super(code, message, printStackTrace, ex);
    }
}
