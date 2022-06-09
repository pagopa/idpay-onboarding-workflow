package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface OnboardingRepository extends ReactiveMongoRepository<Onboarding, String> {

  Mono<Onboarding> findByInitiativeIdAndUserId(String userId, String initiativeId);

}
