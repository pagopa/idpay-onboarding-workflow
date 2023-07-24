package it.gov.pagopa.common.mongo.retry;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface MongoRequestRateTooLargeRetryable {
  /** Maximum number of attempts to retry RequestRateTooLarge mongo exception.
   * If 0 or negative there will not be a limit on the number of attempts */
  long maxRetry() default 0;

  /** Maximum number of milliseconds from the first attempt to retry RequestRateTooLarge mongo exception.
   * If 0 or negative there will not be a limit on the milliseconds elapsed */
  long maxMillisElapsed() default 0;

}
