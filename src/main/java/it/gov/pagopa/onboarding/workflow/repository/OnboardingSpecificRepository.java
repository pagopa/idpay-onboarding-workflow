package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.core.query.Criteria;


public interface OnboardingSpecificRepository {
  List<Onboarding> findByFilter(Criteria criteria, Pageable pageable);
  Criteria getCriteria(String initiativeId, String userId, String status,
      LocalDateTime startDate, LocalDateTime endDate);
  long getCount(Criteria criteria);
  long deleteOnboardingPaged(String initiativeId, int pageSize);

}
