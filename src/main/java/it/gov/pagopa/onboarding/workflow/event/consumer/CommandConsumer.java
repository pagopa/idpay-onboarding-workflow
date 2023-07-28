package it.gov.pagopa.onboarding.workflow.event.consumer;

import it.gov.pagopa.onboarding.workflow.dto.QueueCommandOperationDTO;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.function.Consumer;

@Configuration
public class CommandConsumer {

    @Bean
    public Consumer<QueueCommandOperationDTO> consumerCommand(OnboardingService onboardingService) {
        return onboardingService::processCommand;
    }

}
