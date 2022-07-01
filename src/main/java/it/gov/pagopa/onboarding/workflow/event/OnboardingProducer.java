package it.gov.pagopa.onboarding.workflow.event;

import it.gov.pagopa.onboarding.workflow.dto.mapper.producer.SaveConsentDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.stereotype.Component;

@Component
public class OnboardingProducer {

  @Value("${kafka.topic.onboarding}")
  private String topicOnboarding;
  @Autowired
  StreamBridge streamBridge;

  public void sendSaveConsent(SaveConsentDTO saveConsentDTO){
    streamBridge.send(topicOnboarding, saveConsentDTO);
  }

}
