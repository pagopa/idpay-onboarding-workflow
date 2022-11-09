package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface OnboardingRepository extends MongoRepository<Onboarding, String>, OnboardingSpecificRepository {

  Optional<Onboarding> findByInitiativeIdAndUserId(String initiativeId, String userId);
  List<Onboarding> findByFilter(Criteria criteria, Pageable pageable);
  Criteria getCriteria(String initiativeId, String userId, String status,
      LocalDateTime startDate, LocalDateTime endDate);
  long getCount(Criteria criteria);

}
