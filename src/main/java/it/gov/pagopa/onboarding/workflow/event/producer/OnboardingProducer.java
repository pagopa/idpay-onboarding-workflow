package it.gov.pagopa.onboarding.workflow.event.producer;

import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Component;

@Component
public class OnboardingProducer {

  private final String binder;

  private final StreamBridge streamBridge;

  public OnboardingProducer(StreamBridge streamBridge,
      @Value("${spring.cloud.stream.bindings.onboarding-out-1.binder}") String binder) {
    this.streamBridge = streamBridge;
    this.binder = binder;
  }

  public void sendSaveConsent(OnboardingDTO onboardingDTO, String sessionId) {

    Message<OnboardingDTO> message = MessageBuilder
            .withPayload(onboardingDTO)
            .setHeader("SESSION_ID", sessionId)
            .build();

    streamBridge.send("onboarding-out-1", binder, message);
  }

}




