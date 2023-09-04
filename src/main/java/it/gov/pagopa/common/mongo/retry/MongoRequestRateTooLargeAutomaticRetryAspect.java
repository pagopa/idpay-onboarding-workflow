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
public class MongoRequestRateTooLargeAutomaticRetryAspect {
    private final boolean enabledApi;
    private final long maxRetryApi;
    private final long maxMillisElapsedApi;
    private final boolean enabledBatch;
    private final long maxRetryBatch;
    private final long maxMillisElapsedBatch;

    public MongoRequestRateTooLargeAutomaticRetryAspect(
            @Value("${mongo.request-rate-too-large.api.enabled}") boolean enabledApi,
            @Value("${mongo.request-rate-too-large.api.max-retry:3}") long maxRetryApi,
            @Value("${mongo.request-rate-too-large.api.max-millis-elapsed:0}") long maxMillisElapsedApi,
            @Value("${mongo.request-rate-too-large.batch.enabled}") boolean enabledBatch,
            @Value("${mongo.request-rate-too-large.batch.max-retry}") long maxRetryBatch,
            @Value("${mongo.request-rate-too-large.batch.max-millis-elapsed}") long maxMillisElapsedBatch) {
        this.enabledApi = enabledApi;
        this.maxRetryApi = maxRetryApi;
        this.maxMillisElapsedApi = maxMillisElapsedApi;
        this.enabledBatch = enabledBatch;
        this.maxRetryBatch = maxRetryBatch;
        this.maxMillisElapsedBatch = maxMillisElapsedBatch;
    }

    @Pointcut("within(it.gov.pagopa..*Repository*)")
    public void inRepositoryClass() {
    }

    @Around("inRepositoryClass()")
    public Object decorateRepositoryMethods(ProceedingJoinPoint pjp) throws Throwable {

        if(isNotControllerContext()){
            if(enabledBatch){
                return MongoRequestRateTooLargeRetryableAspect.executeJoinPointRetryable(pjp, maxRetryBatch, maxMillisElapsedBatch);
            } else {
                return pjp.proceed();
            }
        } else {
            if(enabledApi){
                return MongoRequestRateTooLargeRetryableAspect.executeJoinPointRetryable(pjp, maxRetryApi, maxMillisElapsedApi);
            } else {
                return pjp.proceed();
            }
        }

    }

    private static boolean isNotControllerContext() {
        return RequestContextHolder.getRequestAttributes() == null;
    }
}