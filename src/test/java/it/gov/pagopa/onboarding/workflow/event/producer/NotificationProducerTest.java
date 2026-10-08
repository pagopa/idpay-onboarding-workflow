package it.gov.pagopa.onboarding.workflow.event.producer;

import it.gov.pagopa.onboarding.workflow.dto.notification.NotificationQueueDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.messaging.Message;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class NotificationProducerTest {

    @Mock
    private StreamBridge streamBridge;

    private NotificationProducer notificationProducer;

    private static final String BINDER = "testBinder";
    private static final String OPERATION_TYPE = "ONBOARDING";
    private static final String USER_ID = "USERID";
    private static final String INITIATIVE_ID = "INITIATIVEID";
    private static final String SERVICE_ID = "SERVICEID";
    private static final String STATUS = "WAITING_LIST";
    private static final String INITIATIVE_NAME = "INITIATIVE_NAME";

    @BeforeEach
    void setUp() {
        notificationProducer = new NotificationProducer(streamBridge, BINDER);
    }

    @Test
    void sendNotification_shouldSendMessageOnNotificationRequestOut0() {
        NotificationQueueDTO notificationQueueDTO = NotificationQueueDTO.builder()
                .operationType(OPERATION_TYPE)
                .userId(USER_ID)
                .initiativeId(INITIATIVE_ID)
                .serviceId(SERVICE_ID)
                .status(STATUS)
                .initiativeName(INITIATIVE_NAME)
                .build();

        notificationProducer.sendNotification(notificationQueueDTO);

        verify(streamBridge).send(
                eq("notificationRequest-out-0"),
                eq(BINDER),
                argThat((Message<?> msg) -> msg.getPayload().equals(notificationQueueDTO))
        );
    }

    @Test
    void buildMessage_shouldWrapPayload() {
        NotificationQueueDTO notificationQueueDTO = NotificationQueueDTO.builder()
                .operationType(OPERATION_TYPE)
                .userId(USER_ID)
                .initiativeId(INITIATIVE_ID)
                .serviceId(SERVICE_ID)
                .status(STATUS)
                .initiativeName(INITIATIVE_NAME)
                .build();

        Message<NotificationQueueDTO> message = NotificationProducer.buildMessage(notificationQueueDTO);

        assertNotNull(message);
        assertEquals(notificationQueueDTO, message.getPayload());
    }
}

