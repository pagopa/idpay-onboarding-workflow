package it.gov.pagopa.onboarding.workflow.event.consumer;

import it.gov.pagopa.onboarding.workflow.dto.OnboardingNotificationDTO;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.function.Consumer;

import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class OnboardingConsumerTest {

    @Mock
    private OnboardingService onboardingService;

    @InjectMocks
    private OnboardingConsumer onboardingConsumer;

    private Consumer<OnboardingNotificationDTO> consumerCommands;

    private final static String OPERATION_TYPE = "OPERATIONTYPE";
    private final static String USER_ID = "USERID";
    private final static String INITIATIVE_ID = "INITIATIVEID";
    private final static String SERVICE_ID = "SERVICEID";
    private final static String INITIATIVE_NAME = "INITIATIVENAME";
    @BeforeEach
    public void setUp() {
        consumerCommands = onboardingConsumer.consumerOnboarding(onboardingService);
    }

    @Test
    void testConsumerCommands() {
        OnboardingNotificationDTO onboardingNotificationDTO = OnboardingNotificationDTO.builder()
                .userId(USER_ID)
                .initiativeId(INITIATIVE_ID)
                .serviceId(SERVICE_ID)
                .initiativeName(INITIATIVE_NAME)
                .operationType(OPERATION_TYPE)
                .build();
        consumerCommands.accept(onboardingNotificationDTO);
        verify(onboardingService).allowedInitiative(onboardingNotificationDTO);
    }

}
