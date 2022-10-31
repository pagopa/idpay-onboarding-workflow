package it.gov.pagopa.onboarding.workflow.event.producer;

import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.stream.function.StreamBridge;
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

  public void sendSaveConsent(OnboardingDTO onboardingDTO) {
    streamBridge.send("onboarding-out-1", binder, onboardingDTO);
  }

}
