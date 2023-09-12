package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.math.BigDecimal;
import java.time.LocalDate;
import lombok.Data;

@Data
public class InitiativeGeneralDTO   {

  @JsonProperty("budget")
  private BigDecimal budget;

  @JsonProperty("beneficiaryType")
  private String beneficiaryType;

  @JsonProperty("beneficiaryKnown")
  private Boolean beneficiaryKnown;

  @JsonProperty("beneficiaryBudget")
  private BigDecimal beneficiaryBudget;

  @JsonProperty("startDate")
  private LocalDate startDate;

  @JsonProperty("endDate")
  private LocalDate endDate;

  @JsonProperty("rankingStartDate")
  private LocalDate rankingStartDate;

  @JsonProperty("rankingEndDate")
  private LocalDate rankingEndDate;

  @JsonProperty("rankingEnabled")
  private boolean rankingEnabled;

}
