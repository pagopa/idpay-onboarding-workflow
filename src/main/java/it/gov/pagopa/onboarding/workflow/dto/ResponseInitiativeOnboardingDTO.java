package it.gov.pagopa.onboarding.workflow.dto;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

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

