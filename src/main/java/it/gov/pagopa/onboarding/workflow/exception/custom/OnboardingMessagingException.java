package it.gov.pagopa.onboarding.workflow.exception.custom;

public class OnboardingMessagingException extends RuntimeException {
    public OnboardingMessagingException(String message, Throwable cause) {
        super(message, cause);
    }
}