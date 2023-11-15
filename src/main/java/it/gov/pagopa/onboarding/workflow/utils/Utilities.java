package it.gov.pagopa.onboarding.workflow.utils;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.exception.custom.forbidden.InitiativeBudgetExhaustedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.forbidden.InitiativeInvalidException;
import it.gov.pagopa.onboarding.workflow.exception.custom.forbidden.InitiativeOnboardingException;
import it.gov.pagopa.onboarding.workflow.exception.custom.forbidden.UserNotInWhitelistException;
import org.springframework.stereotype.Component;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.*;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.*;

@Component
public class Utilities {

    public String getMessageOnboardingKO(String detail){
        return switch (detail) {
            case OnboardingWorkflowConstants.ERROR_INITIATIVE_END ->
                    OnboardingWorkflowConstants.ERROR_INITIATIVE_END_MSG_AUDIT;
            case OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED ->
                    OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED_MSG_AUDIT;
            case OnboardingWorkflowConstants.ERROR_WHITELIST ->
                    OnboardingWorkflowConstants.ERROR_WHITELIST_MSG_AUDIT;
            case OnboardingWorkflowConstants.ERROR_TECHNICAL ->
                    OnboardingWorkflowConstants.TECHNICAL_ERROR_MSG_AUDIT;
            default -> OnboardingWorkflowConstants.GENERIC_REJECTION_ERROR_MSG_AUDIT;
        };
    }

    public void getOnboardingException(String detail, String initiativeId){
        switch (detail) {
            case OnboardingWorkflowConstants.ERROR_INITIATIVE_END ->
                    throw new InitiativeInvalidException(INITIATIVE_ENDED,
                            String.format(ERROR_INITIATIVE_END_MSG, initiativeId));
            case OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED ->
                    throw new InitiativeBudgetExhaustedException(String.format(ERROR_BUDGET_TERMINATED_MSG, initiativeId));
            case OnboardingWorkflowConstants.ERROR_WHITELIST ->
                    throw new UserNotInWhitelistException(String.format(ERROR_WHITELIST_MSG, initiativeId));
            case OnboardingWorkflowConstants.ERROR_TECHNICAL ->
                    throw new InitiativeOnboardingException(TECHNICAL_ERROR, String.format(TECHNICAL_ERROR_MSG, initiativeId));
            default -> throw new InitiativeOnboardingException(UNSATISFIED_REQUIREMENTS, String.format(UNSATISFIED_REQUIREMENTS_MSG, initiativeId));
        }
    }
}
