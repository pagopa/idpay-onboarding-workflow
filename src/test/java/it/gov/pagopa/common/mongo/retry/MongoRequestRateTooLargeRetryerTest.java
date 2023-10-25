package it.gov.pagopa.common.mongo.retry;

import ch.qos.logback.classic.LoggerContext;
import com.mongodb.MongoQueryException;
import com.mongodb.MongoWriteException;
import com.mongodb.ServerAddress;
import com.mongodb.WriteError;
import it.gov.pagopa.common.mongo.retry.exception.MongoRequestRateTooLargeRetryExpiredException;
import it.gov.pagopa.common.utils.MemoryAppender;
import org.bson.BsonDocument;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mockito;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.mongodb.UncategorizedMongoDbException;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.function.Supplier;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = {MongoRequestRateTooLargeRetryableAspect.class,
    MongoRequestRateTooLargeRetryerTest.TestService.class})
public class MongoRequestRateTooLargeRetryerTest {

  public static final int REQUEST_RATE_TOO_LARGE_MAX_RETRY = 3;
  public static final int REQUEST_RATE_TOO_LARGE_MAX_MILLIS_ELAPSED = 1000;
  @MockBean
  private Supplier<String> dummyServiceMock;

  @Autowired
  private TestService testService;

  private MemoryAppender memoryAppender;

  @BeforeEach
  public void setup() {
    ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(
        MongoRequestRateTooLargeRetryer.class.getName());
    memoryAppender = new MemoryAppender();
    memoryAppender.setContext((LoggerContext) LoggerFactory.getILoggerFactory());
    logger.setLevel(ch.qos.logback.classic.Level.INFO);
    logger.addAppender(memoryAppender);
    memoryAppender.start();
  }

  public static Stream<DataAccessException> buildRetriableExceptions(){
    return Stream.of(
            buildRequestRateTooLargeMongodbException_whenReading(),
            buildRequestRateTooLargeMongodbException_whenWriting(),
            buildRequestRateTooLargeMongodbException_whenBulkWriting()
    );
  }

  /**  Exception thrown when 429 occurs while reading */
  public static UncategorizedMongoDbException buildRequestRateTooLargeMongodbException_whenReading() {
    String mongoFullErrorResponse = """
        {"ok": 0.0, "errmsg": "Error=16500, RetryAfterMs=34,\s
        Details='Response status code does not indicate success: TooManyRequests (429) Substatus: 3200 ActivityId: 46ba3855-bc3b-4670-8609-17e1c2c87778 Reason:\s
        (\\r\\nErrors : [\\r\\n \\"Request rate is large. More Request Units may be needed, so no changes were made. Please retry this request later. Learn more:
         http://aka.ms/cosmosdb-error-429\\"\\r\\n]\\r\\n) ", "code": 16500, "codeName": "RequestRateTooLarge"}
        """;

    MongoQueryException mongoQueryException = new MongoQueryException(
            BsonDocument.parse(mongoFullErrorResponse), new ServerAddress());
    return new UncategorizedMongoDbException(mongoQueryException.getMessage(), mongoQueryException);
  }

  /**  Exception thrown when 429 occurs while writing */
  public static DataIntegrityViolationException buildRequestRateTooLargeMongodbException_whenWriting() {
    String writeErrorMessage = """
            Error=16500, RetryAfterMs=34, Details='Response status code does not indicate success: TooManyRequests (429); Substatus: 3200; ActivityId: 822d212d-5aac-4f5d-a2d4-76d6da7b619e; Reason: (
            Errors : [
              "Request rate is large. More Request Units may be needed, so no changes were made. Please retry this request later. Learn more: http://aka.ms/cosmosdb-error-429"
            ]
            );
            """;
    final MongoWriteException mongoWriteException = new MongoWriteException(
            new WriteError(16500, writeErrorMessage, BsonDocument.parse("{}")), new ServerAddress());
    return new DataIntegrityViolationException(mongoWriteException.getMessage(), mongoWriteException);
  }

  /**  Exception thrown when 429 occurs while bulk writing */
  public static DataIntegrityViolationException buildRequestRateTooLargeMongodbException_whenBulkWriting() {
    String writeErrorMessage = """
            Error=16500, RetryAfterMs=34, Details='Batch write error.'
            """;
    final MongoWriteException mongoWriteException = new MongoWriteException(
            new WriteError(16500, writeErrorMessage, BsonDocument.parse("{}")), new ServerAddress());
    return new DataIntegrityViolationException(mongoWriteException.getMessage(), mongoWriteException);
  }

  public static class TestService {

    @Autowired
    private Supplier<String> dummyService;

    @MongoRequestRateTooLargeRetryable(maxRetry = REQUEST_RATE_TOO_LARGE_MAX_RETRY)
    public String annotatedMethodWithMaxRetry() {
      dummyService.get();
      return "ok1";
    }

    @MongoRequestRateTooLargeRetryable(maxRetry = 120, maxMillisElapsed = REQUEST_RATE_TOO_LARGE_MAX_MILLIS_ELAPSED)
    public String annotatedMethodWithMaxMillisElapsed() {
      dummyService.get();
      return "ok2";
    }
  }

  @ParameterizedTest
  @MethodSource("buildRetriableExceptions")
  void requestRateTooLargeMaxRetry_resultOk(DataAccessException retriableException) {
    // Given
    long[] counter = {0};

    Mockito.doAnswer(invocationOnMock -> {
      if (counter[0]++ < REQUEST_RATE_TOO_LARGE_MAX_RETRY) {
        throw retriableException;
      }
      return "ok1";
    }).when(dummyServiceMock).get();

    // When
    String result = testService.annotatedMethodWithMaxRetry();

    // Then
    assertEquals("ok1", result);
    verify(dummyServiceMock, times(REQUEST_RATE_TOO_LARGE_MAX_RETRY + 1)).get();

    String expectedMessage = "\\[REQUEST_RATE_TOO_LARGE_RETRY]\\[TestService.annotatedMethodWithMaxRetry\\(\\)] Retrying after 34 ms due to RequestRateTooLargeException: attempt %d of %d after .*";
    assertLogMessage(expectedMessage, REQUEST_RATE_TOO_LARGE_MAX_RETRY);

  }

  @ParameterizedTest
  @MethodSource("buildRetriableExceptions")
  void requestRateTooLargeWithMaxMillisElapsed_resultOk(DataAccessException retriableException) {
    // Given
    int[] counter = {0};

    long startTime = System.currentTimeMillis();
    when(dummyServiceMock.get()).thenAnswer(invocation -> {
      counter[0]++;
      if ((System.currentTimeMillis() - startTime) + 40
          < REQUEST_RATE_TOO_LARGE_MAX_MILLIS_ELAPSED) {
        throw retriableException;
      }
      return "ok2";
    });

    // When
    String result = testService.annotatedMethodWithMaxMillisElapsed();
    long elapsedTime = System.currentTimeMillis() - startTime;

    // Then
    assertEquals("ok2", result);
    verify(dummyServiceMock, times(counter[0])).get();
    assertTrue(elapsedTime >= (REQUEST_RATE_TOO_LARGE_MAX_MILLIS_ELAPSED - 50) && elapsedTime <= (
            REQUEST_RATE_TOO_LARGE_MAX_MILLIS_ELAPSED + 50),
        "Obtained elapsed time: " + elapsedTime + "; Minimum expected: "
            + REQUEST_RATE_TOO_LARGE_MAX_MILLIS_ELAPSED);

    String message = "\\[REQUEST_RATE_TOO_LARGE_RETRY]\\[TestService.annotatedMethodWithMaxMillisElapsed\\(\\)] Retrying after 34 ms due to RequestRateTooLargeException: attempt %d of \\d+ after \\d+ ms of max %d ms";
    assertLogMessage(message, REQUEST_RATE_TOO_LARGE_MAX_MILLIS_ELAPSED);
  }

  @ParameterizedTest
  @MethodSource("buildRetriableExceptions")
  void requestRateTooLargeMaxRetry_resultKo(DataAccessException retriableException) {
    when(dummyServiceMock.get()).thenThrow(retriableException);

    try {
      testService.annotatedMethodWithMaxRetry();
      fail();
    } catch (MongoRequestRateTooLargeRetryExpiredException ex) {
      assertEquals(REQUEST_RATE_TOO_LARGE_MAX_RETRY, ex.getMaxRetry());
      assertEquals(REQUEST_RATE_TOO_LARGE_MAX_RETRY + 1, ex.getCounter());
      assertEquals(0, ex.getMaxMillisElapsed());
    }

    // aggiungere assertLog
    verify(dummyServiceMock, times(REQUEST_RATE_TOO_LARGE_MAX_RETRY + 1)).get();
  }

  @ParameterizedTest
  @MethodSource("buildRetriableExceptions")
  void requestRateTooLargeRetryExpiredExceptionWithMaxMillisElapsed(DataAccessException retriableException) {
    when(dummyServiceMock.get()).thenThrow(retriableException);

    assertThrows(MongoRequestRateTooLargeRetryExpiredException.class, () ->
        testService.annotatedMethodWithMaxMillisElapsed()
    );
  }

  @Test
  void uncategorizedMongoDbExceptionNotRequestRateTooLarge() {
    final UncategorizedMongoDbException expectedException = new UncategorizedMongoDbException(
        "not Request Rate Too Large Exception", new Throwable());
    when(dummyServiceMock.get()).thenThrow(expectedException);
    try {
      testService.annotatedMethodWithMaxRetry();
      fail();
    } catch (Exception e) {
      assertSame(expectedException, e);
    }
  }

  @Test
  void requestRateTooLargeMaxRetryNull() {
    //Given
    long[] counter = {0};
    UncategorizedMongoDbException mongoDbException = new UncategorizedMongoDbException(
        "TooManyRequests", new Throwable());

    Mockito.doAnswer(invocationOnMock -> {
      if (counter[0] < REQUEST_RATE_TOO_LARGE_MAX_RETRY) {
        counter[0]++;
        throw mongoDbException;
      }
      return "ok1";
    }).when(dummyServiceMock).get();

    //When
    String result = testService.annotatedMethodWithMaxRetry();

    //Then
    assertEquals("ok1", result);
    verify(dummyServiceMock, times(4)).get();

    String expectedMessage = "\\[REQUEST_RATE_TOO_LARGE_RETRY]\\[TestService.annotatedMethodWithMaxRetry\\(\\)] Retrying for RequestRateTooLargeException: attempt %d of %d after .*";
    assertEquals(3, memoryAppender.getLoggedEvents().size());
    assertLogMessage(expectedMessage, REQUEST_RATE_TOO_LARGE_MAX_RETRY);

  }

  @Test
  void testException() {
    final Error expectedCause = new Error();
    when(dummyServiceMock.get()).thenThrow(expectedCause);
    try {
      testService.annotatedMethodWithMaxRetry();
      fail();
    } catch (IllegalStateException e) {
      assertSame(expectedCause, e.getCause());
    }
  }

  private void assertLogMessage(String expectedMessage, long maxRetryOrMaxMillisElapsed) {
//    assertEquals(counter, memoryAppender.getLoggedEvents().size());
    for (int i = 0; i < memoryAppender.getLoggedEvents().size(); i++) {

      String logMessage = memoryAppender.getLoggedEvents().get(i).getFormattedMessage();
      Assertions.assertTrue(
          logMessage.matches(expectedMessage.formatted(i + 1, maxRetryOrMaxMillisElapsed)),
          logMessage
      );
    }
  }

}
