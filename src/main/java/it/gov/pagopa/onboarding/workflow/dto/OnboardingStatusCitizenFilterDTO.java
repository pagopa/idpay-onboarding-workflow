package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.DateTimeFormat.ISO;

import java.time.Instant;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingStatusCitizenFilterDTO {

  private String userId;
  @DateTimeFormat(iso = ISO.DATE_TIME)
  private Instant startDate;
  private Instant endDate;
  private String status;

}

