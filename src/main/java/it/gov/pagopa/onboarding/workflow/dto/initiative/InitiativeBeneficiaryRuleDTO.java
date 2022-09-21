package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import lombok.Data;

@Data
public class InitiativeBeneficiaryRuleDTO   {

  @JsonProperty("selfDeclarationCriteria")
  private List<SelfDeclarationItemsDTO> selfDeclarationCriteria;

  @JsonProperty("automatedCriteria")
  private List<AutomatedCriteriaDTO> automatedCriteria;

}
