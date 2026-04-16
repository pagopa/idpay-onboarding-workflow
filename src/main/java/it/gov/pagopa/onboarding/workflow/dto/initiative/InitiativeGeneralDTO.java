package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.Valid;
import lombok.Data;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.Map;

@Data
public class InitiativeGeneralDTO   {

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

  @JsonProperty("beneficiaryBudgetMax")
  private BigDecimal beneficiaryBudgetMax;

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

  @JsonProperty("descriptionMap")
  @Valid
  private Map<String, String> descriptionMap;

}
