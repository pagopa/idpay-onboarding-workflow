package it.gov.pagopa.onboarding.workflow.event.consumer;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.List;
import java.util.function.Consumer;

import static org.mockito.Mockito.verify;
@ExtendWith(MockitoExtension.class)
class OutcomeConsumerTest {
    @Mock
    private OnboardingService onboardingService;

    @InjectMocks
    private OutcomeConsumer outcomeConsumer;

    private Consumer<EvaluationDTO> consumerCommands;
    private static final LocalDate OPERATION_DATE = LocalDate.now();
    private static final String USER_ID = "USERID";
    private static final String INITIATIVE_ID = "INITIATIVEID";
    private static final String INITIATIVE_REWARD_TYPE_DISCOUNT = "DISCOUNT";
    private static final String ORGANIZATION_NAME = "TEST_ORGANIZATION_NAME";
    private static final String SERVICE_ID = "SERVICEID";
    @BeforeEach
    void setUp() {
        consumerCommands = outcomeConsumer.consumerOutcome(onboardingService);
    }

    @Test
    void testConsumerCommands() {
        EvaluationDTO evaluationDTO =  new EvaluationDTO(
                USER_ID, null, INITIATIVE_ID, INITIATIVE_ID, OPERATION_DATE, INITIATIVE_ID, OnboardingWorkflowConstants.ONBOARDING_OK,
                OPERATION_DATE.atStartOfDay(), OPERATION_DATE.atStartOfDay(), List.of(),
                500L, INITIATIVE_REWARD_TYPE_DISCOUNT, ORGANIZATION_NAME, false, SERVICE_ID);
        consumerCommands.accept(evaluationDTO);
        verify(onboardingService).completeOnboarding(evaluationDTO);
    }

}
