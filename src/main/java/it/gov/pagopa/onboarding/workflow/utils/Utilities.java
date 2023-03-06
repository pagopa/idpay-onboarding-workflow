package it.gov.pagopa.onboarding.workflow.utils;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;

public class Utilities {

    public String getMessageOnbordingKO(String detail){
        switch(detail){
            case OnboardingWorkflowConstants.ERROR_INITIATIVE_END:
                return OnboardingWorkflowConstants.ERROR_INITIATIVE_END_MSG;
            case OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED:
                return OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED_MSG;
            case OnboardingWorkflowConstants.ERROR_INITIATIVE_SUSPENDED:
                return OnboardingWorkflowConstants.ERROR_INITIATIVE_SUSPENDED_MSG;
            default:
                return OnboardingWorkflowConstants.GENERIC_ERROR_MSG;
        }
    }
}
