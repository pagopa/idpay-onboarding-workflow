package it.gov.pagopa.common.mongo.retry;

import org.aspectj.lang.ProceedingJoinPoint;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.mongodb.UncategorizedMongoDbException;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;

@ExtendWith(MockitoExtension.class)
class MongoRequestRateTooLargeAutomaticRetryAspectTest {

    @Mock
    private ProceedingJoinPoint pjpMock;

    private final int maxRetry = 1;
    private final String expectedResult = "OK";
    @BeforeEach
    void configureRetryMock() throws Throwable {
        int[] counter= {0};
        Mockito.doAnswer(i -> {
            if (counter[0]++ < maxRetry){
                throw MongoRequestRateTooLargeRetryerTest.buildRequestRateTooLargeMongodbException();
            }
            return expectedResult;
        }).when(pjpMock).proceed();
    }

    @AfterEach
    void cleanContext(){
        configureExecutionContext(true);
    }

    //region test batch
    @Test
    void testBatchEnabled() throws Throwable {
        //given
        configureExecutionContext(true);
        MongoRequestRateTooLargeAutomaticRetryAspect aspect = new MongoRequestRateTooLargeAutomaticRetryAspect(false, maxRetry, 1000, true, maxRetry, 1000);

        checkRetryBehaviour(aspect);
    }
    @Test
    void testBatchException() {
        //given
        configureExecutionContext(true);
        MongoRequestRateTooLargeAutomaticRetryAspect aspect = new MongoRequestRateTooLargeAutomaticRetryAspect(false, maxRetry, 1000, false, maxRetry, 1000);

        //when
        //then
        checkException(aspect);
    }

    @Test
    void testBatchDisabledApiEnabled() {
        //given
        configureExecutionContext(true);
        MongoRequestRateTooLargeAutomaticRetryAspect aspect = new MongoRequestRateTooLargeAutomaticRetryAspect(true, maxRetry, 1000, false, maxRetry, 1000);

        //when
        //then
        checkException(aspect);
    }

//endregion

    //region test Api
    @Test
    void testApiEnabled() throws Throwable {
        //given
        configureExecutionContext(false);
        MongoRequestRateTooLargeAutomaticRetryAspect aspect = new MongoRequestRateTooLargeAutomaticRetryAspect(true, maxRetry, 1000, false, maxRetry, 1000);

        //when
        //then
        checkRetryBehaviour(aspect);
    }

    @Test
    void testApiException() {
        //given
        configureExecutionContext(false);
        MongoRequestRateTooLargeAutomaticRetryAspect aspect = new MongoRequestRateTooLargeAutomaticRetryAspect(false, maxRetry, 1000, false, maxRetry, 1000);

        //when
        //then
        checkException(aspect);
    }

    @Test
    void testApiDisabledBatchEnabled() {
        //given
        configureExecutionContext(false);
        MongoRequestRateTooLargeAutomaticRetryAspect aspect = new MongoRequestRateTooLargeAutomaticRetryAspect(false, maxRetry, 1000, true, maxRetry, 1000);

        //when
        //then
        checkException(aspect);
    }

//endregion

    private void checkRetryBehaviour(MongoRequestRateTooLargeAutomaticRetryAspect aspect) throws Throwable {
        //when
        Object result = aspect.decorateRepositoryMethods(pjpMock);

        //then
        Assertions.assertEquals(expectedResult, result);
        Mockito.verify(pjpMock, Mockito.times(maxRetry+1)).proceed();
    }
    private static void configureExecutionContext(boolean isBatch) {
        RequestContextHolder.setRequestAttributes(isBatch ? null : Mockito.mock(RequestAttributes.class));
    }

    private void checkException(MongoRequestRateTooLargeAutomaticRetryAspect aspect) {
        UncategorizedMongoDbException uncategorizedMongoDbException = Assertions.assertThrows(UncategorizedMongoDbException.class, () -> aspect.decorateRepositoryMethods(pjpMock));
        Assertions.assertEquals( MongoRequestRateTooLargeRetryerTest.buildRequestRateTooLargeMongodbException().getMessage() ,uncategorizedMongoDbException.getMessage());
    }
}