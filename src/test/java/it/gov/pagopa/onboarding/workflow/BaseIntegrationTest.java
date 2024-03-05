package it.gov.pagopa.onboarding.workflow;

import com.azure.spring.cloud.autoconfigure.implementation.kafka.AzureEventHubsKafkaOAuth2AutoConfiguration;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.WireMockServer;
import it.gov.pagopa.common.kafka.KafkaTestUtilitiesService;
import it.gov.pagopa.common.mongo.MongoTestUtilitiesService;
import it.gov.pagopa.common.mongo.singleinstance.AutoConfigureSingleInstanceMongodb;
import it.gov.pagopa.common.redis.config.EmbeddedRedisTestConfiguration;
import it.gov.pagopa.common.utils.TestIntegrationUtils;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import jakarta.annotation.PostConstruct;
import org.bson.Document;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.cloud.contract.wiremock.AutoConfigureWireMock;
import org.springframework.context.annotation.Import;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.index.CompoundIndexDefinition;
import org.springframework.data.mongodb.core.index.IndexDefinition;
import org.springframework.kafka.test.context.EmbeddedKafka;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;

import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MalformedObjectNameException;

@SpringBootTest
@EnableAutoConfiguration(exclude = AzureEventHubsKafkaOAuth2AutoConfiguration.class)
@EmbeddedKafka(topics = {
        "${spring.cloud.stream.bindings.consumerOutcome-in-0.destination}",
        "${spring.cloud.stream.bindings.consumerOnboarding-in-0.destination}",
        "${spring.cloud.stream.bindings.consumerCommands-in-0.destination}",
        "${spring.cloud.stream.bindings.onboarding-out-0.destination}",
        "${spring.cloud.stream.bindings.onboarding-out-1.destination}",
}, controlledShutdown = true)
@TestPropertySource(
        properties = {
                // region mongodb
                "logging.level.org.mongodb.driver=WARN",
                "logging.level.de.flapdoodle.embed.mongo.spring.autoconfigure=WARN",
                "de.flapdoodle.mongodb.embedded.version=4.2.24",
                // endregion

                //region wiremock
                "logging.level.WireMock=ERROR",
                "rest-client.group.baseUrl=http://localhost:${wiremock.server.port}",
                "rest-client.initiative.baseUrl=http://localhost:${wiremock.server.port}",
                "rest-client.decrypt.baseUrl=http://localhost:${wiremock.server.port}",
                "rest-client.admissibility.baseUrl=http://localhost:${wiremock.server.port}",
                //endregion

                //region kafka brokers
                "logging.level.org.apache.zookeeper=WARN",
                "logging.level.org.apache.kafka=WARN",
                "logging.level.kafka=WARN",
                "logging.level.state.change.logger=WARN",
                "spring.cloud.stream.kafka.binder.configuration.security.protocol=PLAINTEXT",
                "spring.kafka.bootstrap-servers=${spring.embedded.kafka.brokers}",
                "spring.cloud.stream.binders.kafka-outcome.environment.spring.cloud.stream.kafka.binder.brokers=${spring.embedded.kafka.brokers}",
                "spring.cloud.stream.binders.onboarding-notification.environment.spring.cloud.stream.kafka.binder.brokers=${spring.embedded.kafka.brokers}",
                "spring.cloud.stream.binders.kafka-outcome-out.environment.spring.cloud.stream.kafka.binder.brokers=${spring.embedded.kafka.brokers}",
                "spring.cloud.stream.binders.kafka-commands.environment.spring.cloud.stream.kafka.binder.brokers=${spring.embedded.kafka.brokers}",
                //endregion

                //region service bus
                // mocked replacing it using kafka
                "spring.cloud.azure.servicebus.connection-string=Endpoint=sb://ServiceBusEndpoint;SharedAccessKeyName=sharedAccessKeyName;SharedAccessKey=sharedAccessKey;EntityPath=entityPath",
                "spring.cloud.stream.binders.servicebus-onboarding.type=kafka",
                "spring.cloud.stream.binders.servicebus-onboarding.environment.spring.cloud.stream.kafka.binder.brokers=${spring.embedded.kafka.brokers}",
                "spring.cloud.stream.bindings.admissibilityProcessor-in-0.destination=idpay-onboarding-request",
                //endregion

                //regiondelete
                "app.delete.paginationSize=100",
                "app.delete.delayTime=1000"
                //endregion
        })
@AutoConfigureMockMvc
@AutoConfigureWireMock(stubs = "classpath:/mappings", port = 0)
@Import(EmbeddedRedisTestConfiguration.class)
@AutoConfigureSingleInstanceMongodb
@Disabled
public abstract class BaseIntegrationTest {

    @Autowired
    protected MockMvc mockMvc;
    @Autowired
    protected ObjectMapper objectMapper;

    @Autowired
    protected KafkaTestUtilitiesService kafkaTestUtilitiesService;
    @Autowired
    private MongoTestUtilitiesService mongoTestUtilitiesService;

    @Autowired
    private WireMockServer wireMockServer;

    @Autowired
    protected MongoTemplate mongoTemplate;

    @Value("${spring.data.redis.url}")
    protected String redisUrl;

    @Value("${spring.cloud.stream.bindings.consumerOutcome-in-0.destination}")
    protected String topicConsumerOutcome;
    @Value("${spring.cloud.stream.bindings.consumerOutcome-in-0.group}")
    protected String groupIdConsumerOutcome;

    @Value("${spring.cloud.stream.bindings.consumerOnboarding-in-0.destination}")
    protected String topicConsumerOnboarding;
    @Value("${spring.cloud.stream.bindings.consumerOnboarding-in-0.group}")
    protected String groupIdConsumerOnboarding;

    @Value("${spring.cloud.stream.bindings.onboarding-out-0.destination}")
    protected String topicOnboarding0;
    @Value("${spring.cloud.stream.bindings.onboarding-out-1.destination}")
    protected String topicOnboarding1;

    @BeforeAll
    public static void unregisterPreviouslyKafkaServers() throws MalformedObjectNameException, MBeanRegistrationException, InstanceNotFoundException {
        TestIntegrationUtils.setDefaultTimeZoneAndUnregisterCommonMBean();
    }

    @PostConstruct
    public void logEmbeddedServerConfig() {
        System.out.printf("""
                        ************************
                        Embedded mongo: %s
                        Wiremock HTTP: http://localhost:%s
                        Wiremock HTTPS: %s
                        Embedded kafka: %s
                        Embedded redis: %s
                        ************************
                        """,
                mongoTestUtilitiesService.getMongoUrl(),
                wireMockServer.getOptions().portNumber(),
                wireMockServer.baseUrl(),
                kafkaTestUtilitiesService.getKafkaUrls(),
                redisUrl);
    }

    @BeforeEach
    void configureUniqueIndexes(){
        IndexDefinition index = new CompoundIndexDefinition(new Document().append(Onboarding.Fields.initiativeId, 1).append(Onboarding.Fields.userId, 1))
                .unique();
        mongoTemplate.indexOps(Onboarding.class).ensureIndex(index);
    }
}
