package it.gov.pagopa.common.mongo.retry;

import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.web.context.request.RequestContextHolder;

@Configuration
@EnableAspectJAutoProxy
@Aspect
@Slf4j
public class MongoRequestRateTooLargeRetryWhenNotControllerAspect {

    @Value("${mongo.request-rate-too-large.max-retry:3}")
    private long maxRetry;
    @Value("${mongo.request-rate-too-large.max-millis-elapsed:0}")
    private long maxMillisElapsed;

    @Pointcut("within(it.gov.pagopa..*Repository*)")
    public void inRepositoryClass() {
    }

    @Around("inRepositoryClass()")
    public Object decorateRepositoryMethods(ProceedingJoinPoint pjp) throws Throwable {

            if (isNotControllerContext()) {
                return MongoRequestRateTooLargeRetryableAspect.executeJoinPointRetryable(pjp, maxRetry, maxMillisElapsed);
            } else {
                return pjp.proceed();
            }
    }

    private static boolean isNotControllerContext() {
        return RequestContextHolder.getRequestAttributes() == null;
    }
}
