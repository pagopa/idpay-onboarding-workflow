package it.gov.pagopa.onboarding.workflow.dto;

import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.DateTimeFormat.ISO;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingStatusCitizenFilterDTO {

  private String userId;
  @DateTimeFormat(iso = ISO.DATE_TIME)
  private LocalDateTime startDate;
  private LocalDateTime endDate;
  private String status;

}

