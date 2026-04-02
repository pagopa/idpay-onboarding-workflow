package it.gov.pagopa.onboarding.workflow.dto.web;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.Instant;

@Data
@Builder
@AllArgsConstructor
public class InitiativeGeneralWebDTO {

  @JsonProperty("budget")
  private BigDecimal budget;

  @JsonProperty("beneficiaryType")
  private String beneficiaryType;

  @JsonProperty("familyUnitComposition")
  private String familyUnitComposition;

  @JsonProperty("beneficiaryKnown")
  private Boolean beneficiaryKnown;

  @JsonProperty("beneficiaryBudget")
  private BigDecimal beneficiaryBudget;

  @JsonProperty("startDate")
  private Instant startDate;

  @JsonProperty("endDate")
  private Instant endDate;

  @JsonProperty("rankingStartDate")
  private Instant rankingStartDate;

  @JsonProperty("rankingEndDate")
  private Instant rankingEndDate;

  @JsonProperty("rankingEnabled")
  private Boolean rankingEnabled;

  @JsonProperty("termAndCondition")
  @Valid
 // @NotEmpty(groups = ValidationApiEnabledGroup.class)
  private String termAndCondition;

}
