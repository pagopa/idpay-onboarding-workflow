package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;

@Data
public class InitiativeBeneficiaryRuleDTO   {

  @JsonProperty("selfDeclarationCriteria")
  private List<SelfDeclarationItemsDTO> selfDeclarationCriteria;

  @JsonProperty("automatedCriteria")
  private List<AutomatedCriteriaDTO> automatedCriteria;

  /**
   * PDND Key/Token Id
   */
  private String apiKeyClientId;

  /**
   * PDND Key/Token Assertion
   */
  private String apiKeyClientAssertion;

}
