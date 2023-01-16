package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonProperty;
import it.gov.pagopa.onboarding.workflow.dto.initiative.rule.refund.InitiativeRefundRuleDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.rule.reward.InitiativeRewardRuleDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.rule.trx.InitiativeTrxConditionsDTO;
import java.time.LocalDateTime;
import lombok.Data;

/**
 * InitiativeDTO
 */
@Data
public class InitiativeDTO   {

  @JsonProperty("initiativeId")
  private String initiativeId;

  @JsonProperty("initiativeName")
  private String initiativeName;

  @JsonProperty("organizationId")
  private String organizationId;

  @JsonProperty("status")
  private String status;

  @JsonProperty("creationDate")
  private LocalDateTime creationDate;

  @JsonProperty("updateDate")
  private LocalDateTime updateDate;

  @JsonProperty("general")
  private InitiativeGeneralDTO general;

  @JsonProperty("additionalInfo")
  private InitiativeAdditionalDTO additionalInfo;

  @JsonProperty("beneficiaryRule")
  private InitiativeBeneficiaryRuleDTO beneficiaryRule;

  @JsonProperty("rewardRule")
  private InitiativeRewardRuleDTO rewardRule;

  @JsonProperty("trxRule")
  private InitiativeTrxConditionsDTO trxRule;

  @JsonProperty("refundRule")
  private InitiativeRefundRuleDTO refundRule;
}