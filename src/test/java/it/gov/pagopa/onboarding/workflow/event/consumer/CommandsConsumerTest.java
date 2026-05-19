package it.gov.pagopa.onboarding.workflow.event.consumer;


import it.gov.pagopa.onboarding.workflow.dto.QueueCommandOperationDTO;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.util.function.Consumer;

import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class CommandsConsumerTest {

    @Mock
    private OnboardingService onboardingService;

    @InjectMocks
    private CommandsConsumer commandsConsumer;

    private Consumer<QueueCommandOperationDTO> consumerCommands;

    private static final  String OPERATION_TYPE = "TESTOPERATIONTYPE";
    private static final   String ENTITY_ID = "ENTITYID";
    private static final   Instant OPERATION_TIME = Instant.now();

    @BeforeEach
    void setUp() {
        consumerCommands = commandsConsumer.consumerCommands(onboardingService);
    }

    @Test
    void testConsumerCommands() {
        QueueCommandOperationDTO queueCommandOperationDTO = QueueCommandOperationDTO.builder()
                .operationTime(OPERATION_TIME)
                .entityId(ENTITY_ID)
                .operationType(OPERATION_TYPE)
                .build();
        consumerCommands.accept(queueCommandOperationDTO);
        verify(onboardingService).processCommand(queueCommandOperationDTO);
    }
}
