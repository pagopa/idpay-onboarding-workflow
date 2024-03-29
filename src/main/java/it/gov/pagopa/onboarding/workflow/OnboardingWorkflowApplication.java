package it.gov.pagopa.onboarding.workflow;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;

@SpringBootApplication(scanBasePackages = "it.gov.pagopa")
@EnableCaching
public class OnboardingWorkflowApplication {

  public static void main(String[] args) {
    SpringApplication.run(OnboardingWorkflowApplication.class, args);
  }

}
