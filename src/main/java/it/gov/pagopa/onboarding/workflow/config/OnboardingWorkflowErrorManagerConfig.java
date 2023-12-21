package it.gov.pagopa.onboarding.workflow.config;

import it.gov.pagopa.common.web.dto.ErrorDTO;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class OnboardingWorkflowErrorManagerConfig {
    @Bean
    ErrorDTO defaultErrorDTO() {
        return new ErrorDTO(
                OnboardingWorkflowConstants.ExceptionCode.GENERIC_ERROR,
                "A generic error occurred"
        );
    }

    @Bean
    ErrorDTO tooManyRequestsErrorDTO() {
        return new ErrorDTO(OnboardingWorkflowConstants.ExceptionCode.TOO_MANY_REQUESTS, "Too Many Requests");
    }

    @Bean
    ErrorDTO templateValidationErrorDTO(){
        return new ErrorDTO(OnboardingWorkflowConstants.ExceptionCode.INVALID_REQUEST, null);
    }
}
