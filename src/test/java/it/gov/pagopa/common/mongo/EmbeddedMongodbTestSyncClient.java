package it.gov.pagopa.common.mongo;

import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import it.gov.pagopa.common.mongo.singleinstance.SingleInstanceMongodWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Slf4j
@Service
public class EmbeddedMongodbTestSyncClient implements EmbeddedMongodbTestClient {

    private final String dbName;

    public EmbeddedMongodbTestSyncClient(Environment env) {
        this.dbName = Objects.requireNonNull(env.getProperty("spring.data.mongodb.database"));
    }

    @Override
    public void dropDatabase() {
        String mongodbUrl = getEmbeddedMongdbUrl();

        try(MongoClient mongoClient = MongoClients.create(mongodbUrl)){
            mongoClient.getDatabase(dbName).drop();
        }
    }

    private static String getEmbeddedMongdbUrl() {
        int dbPort = Objects.requireNonNull(SingleInstanceMongodWrapper.singleMongodNet).getPort();
        return "mongodb://localhost:" + dbPort;
    }
}
