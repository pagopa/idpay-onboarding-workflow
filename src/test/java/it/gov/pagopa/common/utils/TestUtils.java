package it.gov.pagopa.common.utils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.gov.pagopa.common.config.JsonConfig;
import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.header.Header;
import org.awaitility.Awaitility;
import org.awaitility.core.ConditionTimeoutException;
import org.junit.jupiter.api.Assertions;
import org.opentest4j.AssertionFailedError;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.ConnectException;
import java.net.Socket;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;

public final class TestUtils {
    private TestUtils() {
    }

    static {
        TimeZone.setDefault(TimeZone.getTimeZone(CommonConstants.ZONEID));
    }

    /**
     * applications's objectMapper
     */
    public static ObjectMapper objectMapper = new JsonConfig().objectMapper();

    /**
     * It will assert not null on all o's fields
     */
    public static void checkNotNullFields(Object o, String... excludedFields) {
        Set<String> excludedFieldsSet = new HashSet<>(Arrays.asList(excludedFields));
        org.springframework.util.ReflectionUtils.doWithFields(o.getClass(),
                f -> {
                    f.setAccessible(true);
                    Assertions.assertNotNull(f.get(o), "The field %s of the input object of type %s is null!".formatted(f.getName(), o.getClass()));
                },
                f -> !excludedFieldsSet.contains(f.getName()));

    }

    /**
     * It will assert null on all o's fields
     */
    public static void checkNullFields(Object o, String... excludedFields) {
        Set<String> excludedFieldsSet = new HashSet<>(Arrays.asList(excludedFields));
        org.springframework.util.ReflectionUtils.doWithFields(o.getClass(),
                f -> {
                    f.setAccessible(true);
                    Object value = f.get(o);
                    Assertions.assertNull(value, "The field %s of the input object of type %s is not null: %s".formatted(f.getName(), o.getClass(), value));
                },
                f -> !excludedFieldsSet.contains(f.getName()));

    }

    /** it will create a BigDecimal with scale2 */
    public static BigDecimal bigDecimalValue(double value) {
        return BigDecimal.valueOf(value).setScale(2, RoundingMode.UNNECESSARY);
    }

    /**
     * It will assert if 2 BigDecimal are equals, ignoring scale
     */
    public static void assertBigDecimalEquals(BigDecimal expected, BigDecimal actual) {
        assertEquals(0, expected.compareTo(actual), "Expected: %s, Obtained: %s".formatted(expected, actual));
    }

    /**
     * To serialize an object as a JSON handling Exception
     */
    public static String jsonSerializer(Object value) {
        try {
            return objectMapper.writeValueAsString(value);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * To read {@link Header} value
     */
    public static String getHeaderValue(ConsumerRecord<String, String> errorMessage, String errorMsgHeaderSrcServer) {
        Header header = errorMessage.headers().lastHeader(errorMsgHeaderSrcServer);
        return header!=null? new String(header.value()) : null;
    }

    /** it will attempt the test until its invocation successfully ends until the configured maxAttempts, waiting for the configured millis between each invocation */
    public static void waitFor(Callable<Boolean> test, Supplier<String> buildTestFailureMessage, int maxAttempts, int millisAttemptDelay) {
        try {
            await()
                    .pollInterval(millisAttemptDelay, TimeUnit.MILLISECONDS)
                    .atMost((long) maxAttempts * millisAttemptDelay, TimeUnit.MILLISECONDS)
                    .until(test);
        } catch (RuntimeException e) {
            Assertions.fail(buildTestFailureMessage.get(), e);
        }
    }

    /** To wait for the configured time */
    public static void wait(long timeout, TimeUnit timeoutUnit) {
        try{
            Awaitility.await().timeout(timeout, timeoutUnit).until(()->false);
        } catch (ConditionTimeoutException ex){
            // Do Nothing
        }
    }

    /** it will truncate the provided datetime field from payload */
    public static String truncateDateTimeField(String payload, String fieldName){
        return payload.replaceAll("(\""+fieldName+"\":\"[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}):[0-9]{2}:[0-9]{2}\\.?[0-9]*\"", "$1:--\"");
    }

    /** it will set to null the provided datetime field from payload */
    public static String setNullFieldValue(String payload, String fieldName) {
        return payload.replaceAll("(\""+fieldName+"\":)(?:[^,}]+)", "$1:null");
    }

    /** It will read a field value from json string */
    public static String readJsonStringFieldValue(String payload, String field) {
        int fieldIndex = payload.indexOf("\""+field+"\"");
        if(fieldIndex>-1){
            String afterField = payload.substring(fieldIndex+field.length()+2);
            final int afterOpeningQuote = afterField.indexOf('"') + 1;
            return afterField.substring(afterOpeningQuote, afterField.indexOf('"', afterOpeningQuote));
        }
        return null;
    }

    /** It will check if the local port is available */
    public static boolean availableLocalPort(int port) {
        try (Socket ignored = new Socket("localhost", port)) {
            return false;
        } catch (ConnectException e) {
            return true;
        } catch (IOException e) {
            throw new IllegalStateException("Error while trying to check open port", e);
        }
    }

    public static void executeParallelUseCases(int executions, List<FailableConsumer<Integer,Exception>> useCases, int parallelism){
        ExecutorService executor= Executors.newFixedThreadPool(parallelism);

        try {
            List<? extends Future<?>> tasks = IntStream.range(0, executions)
                    .mapToObj(i -> executor.submit(() -> {
                        try {
                            useCases.get(i % useCases.size()).accept(i);
                        } catch (Exception e) {
                            throw new IllegalStateException("Unexpected exception thrown during test", e);
                        }
                    }))
                    .toList();

            for (int i = 0; i < tasks.size(); i++) {
                try {
                    tasks.get(i).get();
                } catch (Exception e) {
                    System.err.printf("UseCase %d (execution %d) failed: %n", i % useCases.size(), i);
                    if (e instanceof RuntimeException runtimeException) {
                        throw runtimeException;
                    } else if (e.getCause() instanceof AssertionFailedError assertionFailedError) {
                        throw assertionFailedError;
                    }
                    Assertions.fail(e);
                }
            }
        } finally {
            executor.shutdown();
        }
    }
}
