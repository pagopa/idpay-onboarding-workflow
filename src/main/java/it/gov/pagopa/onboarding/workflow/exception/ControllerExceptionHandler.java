package it.gov.pagopa.onboarding.workflow.exception;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ErrorDTO;
import java.util.ArrayList;
import java.util.List;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
@Slf4j
public class ControllerExceptionHandler {

  @ExceptionHandler({OnboardingWorkflowException.class})
  public ResponseEntity<ErrorDTO> handleException(OnboardingWorkflowException ex) {
    log.error("[ONBOARDING_WORKFLOW][{}] New exception: {}", HttpStatus.valueOf(ex.getCode()), ex.getMessage());
    return new ResponseEntity<>(new ErrorDTO(ex.getCode(), ex.getMessage(), ex.getDetail()),
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
    log.error("[ONBOARDING_WORKFLOW][{}] New exception: {}", HttpStatus.BAD_REQUEST, message);
    return new ResponseEntity<>(
        new ErrorDTO(HttpStatus.BAD_REQUEST.value(), message, OnboardingWorkflowConstants.GENERIC_ERROR),
        HttpStatus.BAD_REQUEST);
  }
}