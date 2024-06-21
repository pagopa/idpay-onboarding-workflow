package it.gov.pagopa.onboarding.workflow.event.producer;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.cloud.stream.function.StreamBridge;

import java.time.LocalDate;
import java.util.List;

import static org.mockito.Mockito.verify;
@ExtendWith(MockitoExtension.class)
class OutcomeProducerTest {
    @Mock
    private StreamBridge streamBridge;

    @InjectMocks
    private OutcomeProducer outcomeProducer;

    private static final LocalDate OPERATION_DATE = LocalDate.now();
    private final static String USER_ID = "USERID";
    private final static String INITIATIVE_ID = "INITIATIVEID";
    private static final String INITIATIVE_REWARD_TYPE_DISCOUNT = "DISCOUNT";
    private static final String ORGANIZATION_NAME = "TEST_ORGANIZATION_NAME";
    @Test
    void testSendSaveOutcome() {
        EvaluationDTO evaluationDTO =  new EvaluationDTO(
                USER_ID, null, INITIATIVE_ID, INITIATIVE_ID, OPERATION_DATE, INITIATIVE_ID, OnboardingWorkflowConstants.ONBOARDING_OK,
                OPERATION_DATE.atStartOfDay(), OPERATION_DATE.atStartOfDay(), List.of(),
                500L, INITIATIVE_REWARD_TYPE_DISCOUNT, ORGANIZATION_NAME, false);
        outcomeProducer.sendOutcome(evaluationDTO);
        verify(streamBridge).send(Mockito.eq("onboarding-out-0"), Mockito.any(), Mockito.eq(evaluationDTO));
    }
}
