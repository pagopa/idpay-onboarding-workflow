package it.gov.pagopa.onboarding.workflow.config;

import it.gov.pagopa.onboarding.workflow.dto.admissibility.InitiativeStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import org.springframework.boot.cache.autoconfigure.RedisCacheManagerBuilderCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.serializer.JacksonJsonRedisSerializer;
import org.springframework.data.redis.serializer.RedisSerializationContext.SerializationPair;
import tools.jackson.databind.ObjectMapper;

import java.time.Duration;

@Configuration
public class RedisConfig {
  @Bean
  public RedisCacheManagerBuilderCustomizer redisCacheManagerBuilderCustomizer() {

    ObjectMapper mapper = new ObjectMapper();

    JacksonJsonRedisSerializer<InitiativeDTO> serializer = new JacksonJsonRedisSerializer<>(mapper, InitiativeDTO.class);

    JacksonJsonRedisSerializer<InitiativeStatusDTO> statusSerializer = new JacksonJsonRedisSerializer<>(mapper, InitiativeStatusDTO.class);

    return builder -> builder
        .withCacheConfiguration("initiativeBeneficiaryView",
            RedisCacheConfiguration.defaultCacheConfig().entryTtl(Duration.ofDays(1))
                .serializeValuesWith(
                    SerializationPair.fromSerializer(serializer)))
        .withCacheConfiguration("initiativeIdBudget",
            RedisCacheConfiguration.defaultCacheConfig().entryTtl(Duration.ofSeconds(30))
                .serializeValuesWith(
                        SerializationPair.fromSerializer(statusSerializer)));
  }
}
