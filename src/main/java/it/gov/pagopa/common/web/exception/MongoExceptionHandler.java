package it.gov.pagopa.common.web.exception;

import it.gov.pagopa.common.web.dto.ErrorDTO;
import jakarta.servlet.http.HttpServletRequest;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.data.mongodb.UncategorizedMongoDbException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.ResponseEntity.BodyBuilder;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@RestControllerAdvice
@Slf4j
@Order(Ordered.HIGHEST_PRECEDENCE)
public class MongoExceptionHandler {
  private static final Pattern RETRY_AFTER_MS_PATTERN = Pattern.compile("RetryAfterMs=(\\d+)");

  @ExceptionHandler(UncategorizedMongoDbException.class)
  protected ResponseEntity<ErrorDTO> handleUncategorizedMongoDbException(
      UncategorizedMongoDbException ex, HttpServletRequest request) {

    if(isRequestRateTooLargeException(ex)) {
      String message = ex.getMessage();

      log.info("A MongoQueryException (RequestRateTooLarge) occurred handling request {}: HttpStatus 429 - {}",
          ErrorManager.getRequestDetails(request), message);
      log.debug("Something went wrong while accessing MongoDB", ex);

      final BodyBuilder bodyBuilder = ResponseEntity.status(HttpStatus.TOO_MANY_REQUESTS)
          .contentType(MediaType.APPLICATION_JSON);

      Long retryAfterMs = getRetryAfterMs(ex);
      if(retryAfterMs != null){
        long retryAfter = (long) Math.ceil((double) retryAfterMs/1000);
        bodyBuilder.header(HttpHeaders.RETRY_AFTER, String.valueOf(retryAfter))
            .header("Retry-After-Ms", String.valueOf(retryAfterMs));
      }

      return bodyBuilder
          .body(new ErrorDTO("TOO_MANY_REQUESTS", "TOO_MANY_REQUESTS", ""));
    }else {
      throw ex;
    }
  }

  private static Long getRetryAfterMs(UncategorizedMongoDbException ex) {
    Matcher matcher = RETRY_AFTER_MS_PATTERN.matcher(ex.getMessage());
    if(matcher.find()){
      return Long.parseLong(matcher.group(1));
    }
    return null;
  }

  private static boolean isRequestRateTooLargeException(UncategorizedMongoDbException ex) {
    return ex.getMessage().contains("RequestRateTooLarge");
  }
}
