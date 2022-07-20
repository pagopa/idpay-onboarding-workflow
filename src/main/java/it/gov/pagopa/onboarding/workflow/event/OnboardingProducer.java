package it.gov.pagopa.onboarding.workflow.event;

import it.gov.pagopa.onboarding.workflow.dto.mapper.producer.SaveConsentDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.stereotype.Component;

@Component
public class OnboardingProducer {

  @Value("${spring.cloud.stream.bindings.onboardingQueue-out-0.binder}")
  private String onboardingBinder;

  @Autowired
  StreamBridge streamBridge;

  public void sendSaveConsent(SaveConsentDTO saveConsentDTO){
    streamBridge.send("onboardingQueue-out-0", onboardingBinder, saveConsentDTO);
  }

}
