package it.gov.pagopa.common.mongo.retry;

import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;

@Configuration
@EnableAspectJAutoProxy
@Aspect
@Slf4j
public class MongoRequestRateTooLargeRetryableAspect {

  @Autowired
  private ApplicationContext applicationContext;

  @Around("@annotation(mongoRequestRateTooLargeRetryable)")
  public Object mongoRequestTooLargeRetryable(ProceedingJoinPoint pjp,
      MongoRequestRateTooLargeRetryable mongoRequestRateTooLargeRetryable)
      throws InterruptedException {
    return MongoRequestRateTooLargeRetryer.execute(() -> {
      try {
        return pjp.proceed();
      } catch (RuntimeException e) {
        throw e;
      } catch (Throwable e) {
        throw new IllegalStateException(
            "[REQUEST_RATE_TOO_LARGE_RETRY] Something went wrong while executing MongoRequestRateTooLargeRetryable annotated method",
            e);
      }
    }, mongoRequestRateTooLargeRetryable.maxRetry(), mongoRequestRateTooLargeRetryable.maxMillisElapsed());
  }

}
