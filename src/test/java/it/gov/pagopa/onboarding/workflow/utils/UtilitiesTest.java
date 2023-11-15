package it.gov.pagopa.onboarding.workflow.utils;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.exception.custom.forbidden.InitiativeBudgetExhaustedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.forbidden.InitiativeInvalidException;
import it.gov.pagopa.onboarding.workflow.exception.custom.forbidden.InitiativeOnboardingException;
import it.gov.pagopa.onboarding.workflow.exception.custom.forbidden.UserNotInWhitelistException;
import org.junit.jupiter.api.Test;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.*;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.GENERIC_ERROR_MSG;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class UtilitiesTest {

    private static final String INITIATIVE_ID = "INITIATIVE_ID";
    private final Utilities utilities = new Utilities();

    @Test
    void detailInitiativeEnd(){
        String message = utilities.getMessageOnboardingKO(OnboardingWorkflowConstants.ERROR_INITIATIVE_END);
        assertEquals(OnboardingWorkflowConstants.ERROR_INITIATIVE_END_MSG_AUDIT, message);
    }

    @Test
    void detailBudgetTerminated(){
        String message = utilities.getMessageOnboardingKO(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED);
        assertEquals(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED_MSG_AUDIT, message);
    }

    @Test
    void detailWhitelist(){
        String message = utilities.getMessageOnboardingKO(OnboardingWorkflowConstants.ERROR_WHITELIST);
        assertEquals(OnboardingWorkflowConstants.ERROR_WHITELIST_MSG_AUDIT, message);
    }

    @Test
    void detailGenericError(){
        String message = utilities.getMessageOnboardingKO("Generic error");
        assertEquals(GENERIC_ERROR_MSG, message);
    }

    @Test
    void initiativeEndedException(){
        InitiativeInvalidException exception = assertThrows(InitiativeInvalidException.class,
                () -> utilities.getOnboardingException(OnboardingWorkflowConstants.ERROR_INITIATIVE_END, INITIATIVE_ID));

        assertEquals(INITIATIVE_ENDED, exception.getCode());
    }

    @Test
    void budgetTerminatedException(){
        InitiativeBudgetExhaustedException exception = assertThrows(InitiativeBudgetExhaustedException.class,
                () -> utilities.getOnboardingException(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED, INITIATIVE_ID));

        assertEquals(BUDGET_EXHAUSTED, exception.getCode());
    }

    @Test
    void errorWhitelistException(){
        UserNotInWhitelistException exception = assertThrows(UserNotInWhitelistException.class,
                () -> utilities.getOnboardingException(OnboardingWorkflowConstants.ERROR_INITIATIVE_END, INITIATIVE_ID));

        assertEquals(USER_NOT_IN_WHITELIST, exception.getCode());
    }

    @Test
    void genericOnboardingErrorException(){
        InitiativeOnboardingException exception = assertThrows(InitiativeOnboardingException.class,
                () -> utilities.getOnboardingException(OnboardingWorkflowConstants.ERROR_INITIATIVE_END, INITIATIVE_ID));

        assertEquals(GENERIC_ERROR, exception.getCode());
    }

}
