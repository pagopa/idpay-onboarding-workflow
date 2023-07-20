package it.gov.pagopa.common.mongo.retry;

import java.util.function.Function;

public interface MongoRequestTooLargeRetryablePayloadBuilder<T> extends Function<T, String> {
}
