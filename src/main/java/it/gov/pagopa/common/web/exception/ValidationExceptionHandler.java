package it.gov.pagopa.common.web.exception;

import it.gov.pagopa.common.web.dto.ErrorDTO;
import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.lang.Nullable;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingRequestHeaderException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.Optional;
import java.util.stream.Collectors;

@RestControllerAdvice
@Slf4j
@Order(Ordered.HIGHEST_PRECEDENCE)
public class ValidationExceptionHandler {

    private final ErrorDTO templateValidationErrorDTO;

    public ValidationExceptionHandler(@Nullable ErrorDTO templateValidationErrorDTO) {
        this.templateValidationErrorDTO = Optional.ofNullable(templateValidationErrorDTO)
                .orElse(new ErrorDTO("INVALID_REQUEST", "Invalid request"));
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public ErrorDTO handleValidationExceptions(
            MethodArgumentNotValidException ex, HttpServletRequest request) {

        String message = ex.getBindingResult().getAllErrors().stream()
                .map(error -> {
                    String fieldName = ((FieldError) error).getField();
                    String errorMessage = error.getDefaultMessage();
                    return String.format("[%s]: %s", fieldName, errorMessage);
                }).collect(Collectors.joining("; "));

        log.info("A MethodArgumentNotValidException occurred handling request {}: HttpStatus 400 - {}",
                ErrorManager.getRequestDetails(request), message);
        log.debug("Something went wrong while validating http request", ex);

        return new ErrorDTO(templateValidationErrorDTO.getCode(), message);
    }

    @ExceptionHandler(MissingRequestHeaderException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public ErrorDTO handleMissingRequestHeaderExceptions(
            MissingRequestHeaderException ex, HttpServletRequest request) {

        String message = ex.getMessage();

        log.info("A MissingRequestHeaderException occurred handling request {}: HttpStatus 400 - {}",
                ErrorManager.getRequestDetails(request), message);
        log.debug("Something went wrong handling request", ex);

        return new ErrorDTO(templateValidationErrorDTO.getCode(), message);
    }
}
