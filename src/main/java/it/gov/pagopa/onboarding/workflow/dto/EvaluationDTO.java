package it.gov.pagopa.onboarding.workflow.dto;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class EvaluationDTO {

  @NotEmpty
  private String userId;
  @NotEmpty
  private String initiativeId;
  private String initiativeName;
  private LocalDateTime initiativeEndDate;
  private String organizationId;
  @NotEmpty
  private String status;
  @NotNull
  private LocalDateTime admissibilityCheckDate;
  @NotNull
  private List<OnboardingRejectionReason> onboardingRejectionReasons;
  private BigDecimal beneficiaryBudget;
  private String serviceId;
}