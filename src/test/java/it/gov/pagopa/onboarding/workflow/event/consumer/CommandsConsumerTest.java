package it.gov.pagopa.onboarding.workflow.event.consumer;


import it.gov.pagopa.onboarding.workflow.dto.QueueCommandOperationDTO;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.function.Consumer;

import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class CommandsConsumerTest {

    @Mock
    private OnboardingService onboardingService;

    @InjectMocks
    private CommandsConsumer commandsConsumer;

    private Consumer<QueueCommandOperationDTO> consumerCommands;

    private final static String OPERATION_TYPE = "TESTOPERATIONTYPE";
    private final static  String ENTITY_ID = "ENTITYID";
    private final static  LocalDateTime OPERATION_TIME = LocalDateTime.now();

    @BeforeEach
    public void setUp() {
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
