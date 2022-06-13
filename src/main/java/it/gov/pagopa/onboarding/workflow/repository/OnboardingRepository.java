package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import java.util.Optional;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface OnboardingRepository extends MongoRepository<Onboarding, String> {

  Optional<Onboarding> findByInitiativeIdAndUserId(String userId, String initiativeId);

}
