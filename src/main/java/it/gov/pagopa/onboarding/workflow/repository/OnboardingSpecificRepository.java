package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.core.query.Criteria;

import java.time.LocalDateTime;
import java.util.List;


public interface OnboardingSpecificRepository {
  List<Onboarding> findByFilter(Criteria criteria);
  Criteria getCriteria(String initiativeId, String userId, String status,
      LocalDateTime startDate, LocalDateTime endDate);
  long getCount(Criteria criteria);
  List<Onboarding> deletePaged(String initiativeId, int pageSize);

}
