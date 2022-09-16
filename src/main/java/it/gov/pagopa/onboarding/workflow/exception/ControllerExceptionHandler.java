package it.gov.pagopa.onboarding.workflow.exception;

import it.gov.pagopa.onboarding.workflow.dto.ErrorDTO;
import java.util.ArrayList;
import java.util.List;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
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
    List<String> errors = new ArrayList<>();
    ex.getBindingResult().getAllErrors().forEach(error -> {
      String fieldName = ((FieldError) error).getField();
      String errorMessage = error.getDefaultMessage();
      errors.add(String.format("[%s]: %s", fieldName, errorMessage));
    });
    String message = String.join(" - ", errors);
    return new ResponseEntity<>(
        new ErrorDTO(HttpStatus.BAD_REQUEST.value(), message),
        HttpStatus.BAD_REQUEST);
  }
}
