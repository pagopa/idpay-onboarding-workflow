package it.gov.pagopa.onboarding.workflow.exception;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;

class OnboardingWorkflowExceptionTest {
    @Test
    void testNoHttpCode(){
        // Given
        String expectedMessage = "TEST";
        String expectedDetails = "TESTDETAILS";
        RuntimeException expectedCause = new RuntimeException();

        // When
        OnboardingWorkflowException result = new OnboardingWorkflowException(-1, expectedMessage, expectedDetails, expectedCause);

        // Then
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, result.getHttpStatus());
        Assertions.assertSame(expectedMessage, result.getMessage());
        Assertions.assertSame(expectedCause, result.getCause());
    }
}
