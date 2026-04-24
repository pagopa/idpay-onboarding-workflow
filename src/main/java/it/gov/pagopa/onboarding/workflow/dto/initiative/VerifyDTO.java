package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class VerifyDTO {

  @JsonProperty("code")
  private String code;

  @JsonProperty("verify")
  private boolean verify;

  @JsonProperty("thresholdCode")
  private String thresholdCode;

  @JsonProperty("beneficiaryBudgetCentsMin")
  private long beneficiaryBudgetCentsMin;

  @JsonProperty("beneficiaryBudgetCentsMax")
  private long beneficiaryBudgetCentsMax;

  @JsonProperty("blockingVerify")
  private boolean blockingVerify;


}
