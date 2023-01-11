package it.gov.pagopa.onboarding.workflow.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import java.time.Duration;
import org.springframework.boot.autoconfigure.cache.RedisCacheManagerBuilderCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.serializer.Jackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.RedisSerializationContext.SerializationPair;

@Configuration
public class RedisConfig {
  @Bean
  public RedisCacheManagerBuilderCustomizer redisCacheManagerBuilderCustomizer() {

    ObjectMapper mapper = new ObjectMapper();
    mapper.findAndRegisterModules();

    Jackson2JsonRedisSerializer<InitiativeDTO> serializer = new Jackson2JsonRedisSerializer<>(InitiativeDTO.class);
    serializer.setObjectMapper(mapper);

    return builder -> builder
        .withCacheConfiguration("initiativeBeneficiaryView",
            RedisCacheConfiguration.defaultCacheConfig().entryTtl(Duration.ofDays(1))
                .serializeValuesWith(
                    SerializationPair.fromSerializer(serializer)));
  }
}
