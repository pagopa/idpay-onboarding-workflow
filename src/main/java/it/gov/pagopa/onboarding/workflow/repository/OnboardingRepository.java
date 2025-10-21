package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OnboardingRepository extends MongoRepository<Onboarding, String>, OnboardingSpecificRepository {

  List<Onboarding> findByInitiativeIdAndFamilyId(String initiativeId, String familyId);

  int countByInitiativeIdAndStatus(String initiativeId, String status);

  List<Onboarding> deleteByInitiativeId(String initiativeId);
}
