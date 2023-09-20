package it.gov.pagopa.common.mongo.retry;

import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;

@Configuration
@EnableAspectJAutoProxy
@Aspect
@Slf4j
public class MongoRequestRateTooLargeRetryableAspect {

    @Around("@annotation(mongoRequestRateTooLargeRetryable)")
    public Object mongoRequestTooLargeRetryable(ProceedingJoinPoint pjp,
                                                MongoRequestRateTooLargeRetryable mongoRequestRateTooLargeRetryable)
            throws InterruptedException {
        return executeJoinPointRetryable(pjp, mongoRequestRateTooLargeRetryable.maxRetry(),
                mongoRequestRateTooLargeRetryable.maxMillisElapsed());
    }

    public static Object executeJoinPointRetryable(ProceedingJoinPoint pjp, long maxRetry, long maxMillisElapsed)
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
        }, maxRetry, maxMillisElapsed);
    }

}