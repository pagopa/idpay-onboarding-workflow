package it.gov.pagopa.onboarding.workflow.config;

import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestClient;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestClient;
import it.gov.pagopa.onboarding.workflow.connector.decrypt.DecryptRest;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableFeignClients(clients = {
    DecryptRest.class,
    InitiativeRestClient.class,
    AdmissibilityRestClient.class
})
public class OnboardingWorkflowConfig {

}
