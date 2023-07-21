package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.common.mongo.retry.MongoRequestRateTooLargeRetryable;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import java.util.Optional;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OnboardingRepository extends MongoRepository<Onboarding, String>, OnboardingSpecificRepository {

  List<Onboarding> findByInitiativeIdAndFamilyId(String initiativeId, String familyId);

  int countByInitiativeIdAndStatus(String initiativeId, String status);

  @MongoRequestRateTooLargeRetryable(maxRetry = 10)
  default Optional<Onboarding> findByIdRetryable(String id) {
    return findById(id);
  }
  @MongoRequestRateTooLargeRetryable(maxRetry = 10)
  default <S extends Onboarding> S saveRetryable(S entity){
    return save(entity);
  }
}
