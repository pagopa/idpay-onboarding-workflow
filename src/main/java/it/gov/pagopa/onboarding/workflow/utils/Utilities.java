package it.gov.pagopa.onboarding.workflow.utils;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import org.springframework.stereotype.Component;

@Component
public class Utilities {

    public String getMessageOnboardingKO(String detail){
         return switch (detail) {
            case OnboardingWorkflowConstants.ERROR_INITIATIVE_END ->
                    OnboardingWorkflowConstants.ERROR_INITIATIVE_END_MSG;
            case OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED ->
                    OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED_MSG;
            case OnboardingWorkflowConstants.ERROR_INITIATIVE_SUSPENDED ->
                    OnboardingWorkflowConstants.ERROR_INITIATIVE_SUSPENDED_MSG;
            default -> OnboardingWorkflowConstants.GENERIC_ERROR_MSG;
        };
    }
}
