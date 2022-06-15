package it.gov.pagopa.onboarding.workflow.exception;

import it.gov.pagopa.onboarding.workflow.dto.ErrorDTO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class ControllerExceptionHandler {

  @ExceptionHandler({OnboardingWorkflowException.class})
  public ResponseEntity<ErrorDTO> handleException(OnboardingWorkflowException ex) {
    return new ResponseEntity<>(new ErrorDTO(ex.getCode(), ex.getMessage()),
        HttpStatus.valueOf(ex.getCode()));
  }

  @ExceptionHandler(MethodArgumentNotValidException.class)
  public ResponseEntity<ErrorDTO> handleValidationExceptions(
      MethodArgumentNotValidException ex) {
    return new ResponseEntity<>(
        new ErrorDTO(HttpStatus.BAD_REQUEST.value(), "The field initiativeId cannot be blank!"),
        HttpStatus.BAD_REQUEST);
  }
}
