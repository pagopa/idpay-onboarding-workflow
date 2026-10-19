package it.gov.pagopa.onboarding.workflow.event.producer;

import it.gov.pagopa.onboarding.workflow.dto.notification.NotificationQueueDTO;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Component;

@Component
public class NotificationProducer {

  private final String binder;
  private final StreamBridge streamBridge;

  public NotificationProducer(StreamBridge streamBridge,
      @Value("${spring.cloud.stream.bindings.notificationRequest-out-0.binder}") String binder) {
    this.streamBridge = streamBridge;
    this.binder = binder;
  }

  public boolean sendNotification(NotificationQueueDTO notificationQueueDTO) {
    return streamBridge.send("notificationRequest-out-0", binder, buildMessage(notificationQueueDTO));
  }

  public static Message<NotificationQueueDTO> buildMessage(NotificationQueueDTO notificationQueueDTO) {
    return MessageBuilder.withPayload(notificationQueueDTO).build();
  }
}
