package it.gov.pagopa.common.mongo.retry;

import it.gov.pagopa.common.mongo.retry.exception.MongoRequestRateTooLargeRetryExpiredException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;

import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.lang.Thread.sleep;

@Slf4j
public final class MongoRequestRateTooLargeRetryer {

  private MongoRequestRateTooLargeRetryer() {
  }

  private static final Pattern RETRY_AFTER_MS_PATTERN = Pattern.compile("RetryAfterMs=(\\d+)");

  public static <T> T execute(Supplier<T> logic, long maxRetry, long maxMillisElapsed)
      throws InterruptedException {
    long counter = 0;
    long startime = System.currentTimeMillis();
    while (true) {
      try {
        return logic.get();
      } catch (DataAccessException e) {
        handleMongoException(e, maxRetry, ++counter, maxMillisElapsed, startime);
      }
    }
  }

  private static void handleMongoException(DataAccessException e, long maxRetry,
      long counter, long maxMillisElapsed, long startime)
      throws InterruptedException {
    long millisElapsed = System.currentTimeMillis() - startime;

    if (isRequestRateTooLargeException(e)) {
      Long retryAfterMs = getRetryAfterMs(e);

      if(retryAfterMs != null){
        millisElapsed+=retryAfterMs;
      }

      if ((maxRetry <= 0 || counter <= maxRetry)
          && (maxMillisElapsed <= 0 || millisElapsed <= maxMillisElapsed)) {

        if (retryAfterMs != null) {
          log.info(
              "[REQUEST_RATE_TOO_LARGE_RETRY] Retrying after {} ms due to RequestRateTooLargeException: attempt {} of {} after {} ms of max {} ms",
              retryAfterMs, counter, maxRetry, millisElapsed, maxMillisElapsed);
          sleep(retryAfterMs);
        } else {
          log.info(
              "[REQUEST_RATE_TOO_LARGE_RETRY] Retrying for RequestRateTooLargeException: attempt {} of {} after {} ms of max {} ms",
              counter, maxRetry, millisElapsed, maxMillisElapsed);
        }
      } else {
        throw new MongoRequestRateTooLargeRetryExpiredException(maxRetry, counter, maxMillisElapsed,
            millisElapsed, retryAfterMs, e);
      }
    } else {
      throw e;
    }
  }


  public static Long getRetryAfterMs(DataAccessException ex) {
    Matcher matcher = RETRY_AFTER_MS_PATTERN.matcher(ex.getMessage());
    if (matcher.find()) {
      return Long.parseLong(matcher.group(1));
    }
    return null;
  }

  public static boolean isRequestRateTooLargeException(DataAccessException ex) {
    return ex.getMessage().contains("TooManyRequests");
  }

}
