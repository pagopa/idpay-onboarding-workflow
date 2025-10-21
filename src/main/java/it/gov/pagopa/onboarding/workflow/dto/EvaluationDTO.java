package it.gov.pagopa.onboarding.workflow.dto;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class EvaluationDTO {

  @NotEmpty
  private String userId;
  private String familyId;
  @NotEmpty
  private String initiativeId;
  private String initiativeName;
  private LocalDate initiativeEndDate;
  private String organizationId;
  @NotEmpty
  private String status;
  @NotNull
  private LocalDateTime admissibilityCheckDate;
  private LocalDateTime criteriaConsensusTimestamp;
  @NotNull
  private List<OnboardingRejectionReason> onboardingRejectionReasons;
  private Long beneficiaryBudgetCents;
  private String initiativeRewardType;
  private String organizationName;
  private Boolean isLogoPresent;
  private String serviceId;
}