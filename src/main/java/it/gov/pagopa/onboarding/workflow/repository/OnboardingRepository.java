package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface OnboardingRepository extends MongoRepository<Onboarding, String>, OnboardingSpecificRepository {

  Optional<Onboarding> findByInitiativeIdAndUserId(String initiativeId, String userId);

  Long getCountOnboardedCitizen(String initiativeId, String status);

}
