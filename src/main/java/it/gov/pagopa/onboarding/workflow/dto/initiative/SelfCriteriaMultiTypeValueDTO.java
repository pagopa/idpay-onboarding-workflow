package it.gov.pagopa.onboarding.workflow.dto.initiative;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SelfCriteriaMultiTypeValueDTO {

  private String description;

  private String subDescription;

  private String value;

  private boolean verify;

  private String thresholdCode;

  private Long beneficiaryBudgetCentsMin;

  private Long beneficiaryBudgetCentsMax;

  private boolean blockingVerify;

}
