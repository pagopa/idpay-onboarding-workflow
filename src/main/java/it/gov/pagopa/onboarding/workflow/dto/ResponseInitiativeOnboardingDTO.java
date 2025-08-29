package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ResponseInitiativeOnboardingDTO {
  private List<OnboardingStatusCitizenDTO> onboardingStatusCitizenDTOList;
  private int pageNo;
  private int pageSize;
  private int totalElements;
  private int totalPages;
}

