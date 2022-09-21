package it.gov.pagopa.onboarding.workflow.event.producer;

import it.gov.pagopa.onboarding.workflow.dto.SaveConsentDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.stereotype.Component;

@Component
public class OnboardingProducer {

  @Value("${spring.cloud.stream.bindings.onboardingProducer-out-0.binder}")
  private String onboardingBinder;

  @Autowired
  private StreamBridge streamBridge;

  public void sendSaveConsent(SaveConsentDTO saveConsentDTO){
    streamBridge.send("onboardingProducer-out-0", onboardingBinder, saveConsentDTO);
  }

}
