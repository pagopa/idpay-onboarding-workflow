package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import java.time.LocalDateTime;
import org.springframework.data.domain.Pageable;
import reactor.core.publisher.Flux;


public interface OnboardingSpecificRepository {
  Flux<Onboarding> findByFilter(String initiativeId,String userId, String status, LocalDateTime startDate, LocalDateTime endDate, Pageable pageable);
}
