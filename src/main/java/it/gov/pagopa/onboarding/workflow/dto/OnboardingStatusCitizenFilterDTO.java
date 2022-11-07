package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.domain.Pageable;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingStatusCitizenFilterDTO {

  private String initiativeId;
  private String userId;
  private String startDate;
  private String endDate;
  private String status;
  private String onboardingDate;
  private Pageable pageable;

}

