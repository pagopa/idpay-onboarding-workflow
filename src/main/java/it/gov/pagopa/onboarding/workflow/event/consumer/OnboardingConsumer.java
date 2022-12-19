package it.gov.pagopa.onboarding.workflow.event.consumer;

import it.gov.pagopa.onboarding.workflow.dto.OnboardingNotificationDTO;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import java.util.function.Consumer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class OnboardingConsumer {

  @Bean
  public Consumer<OnboardingNotificationDTO> consumerOnboarding(OnboardingService onboardingService) {
    return onboardingService::allowedInitiative;
  }

}
