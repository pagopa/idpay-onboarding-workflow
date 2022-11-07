package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface OnboardingRepositoryReactive extends ReactiveMongoRepository<Onboarding,String>, OnboardingSpecificRepository{

}
