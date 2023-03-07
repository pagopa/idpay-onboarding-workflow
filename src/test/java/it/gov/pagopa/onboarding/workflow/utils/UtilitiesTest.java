package it.gov.pagopa.onboarding.workflow.utils;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;

class UtilitiesTest {

    private final Utilities utilities = new Utilities();

    @Test
    void detailInitiativeEnd(){
        String message = utilities.getMessageOnboardingKO(OnboardingWorkflowConstants.ERROR_INITIATIVE_END);
        assertEquals(OnboardingWorkflowConstants.ERROR_INITIATIVE_END_MSG, message);
    }

    @Test
    void detailBudgetTerminated(){
        String message = utilities.getMessageOnboardingKO(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED);
        assertEquals(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED_MSG, message);
    }

    @Test
    void detailInitiativeSuspended(){
        String message = utilities.getMessageOnboardingKO(OnboardingWorkflowConstants.ERROR_INITIATIVE_SUSPENDED);
        assertEquals(OnboardingWorkflowConstants.ERROR_INITIATIVE_SUSPENDED_MSG, message);
    }

    @Test
    void detailGenericError(){
        String message = utilities.getMessageOnboardingKO("Generic error");
        assertEquals(OnboardingWorkflowConstants.GENERIC_ERROR_MSG, message);
    }

}
