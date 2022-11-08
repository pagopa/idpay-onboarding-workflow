package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.domain.Pageable;


public interface OnboardingSpecificRepository {
  List<Onboarding> findByFilter(String initiativeId,String userId, String status, LocalDateTime startDate, LocalDateTime endDate, Pageable pageable);
}
