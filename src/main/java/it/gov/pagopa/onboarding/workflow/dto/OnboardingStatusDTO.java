package it.gov.pagopa.onboarding.workflow.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingStatusDTO {

  private String status;
  private LocalDateTime statusDate;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private LocalDateTime onboardingOkDate;

}
