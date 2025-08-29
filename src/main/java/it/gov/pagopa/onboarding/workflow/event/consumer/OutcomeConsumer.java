package it.gov.pagopa.onboarding.workflow.event.consumer;

import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.function.Consumer;

@Configuration
public class OutcomeConsumer {

  @Bean
  public Consumer<EvaluationDTO> consumerOutcome(OnboardingService onboardingService) {
    return onboardingService::completeOnboarding;
  }

}
