package it.gov.pagopa.onboarding.workflow.config;

import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestClient;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableFeignClients(clients = {InitiativeRestClient.class})
public class OnboardingWorkflowConfig {}
