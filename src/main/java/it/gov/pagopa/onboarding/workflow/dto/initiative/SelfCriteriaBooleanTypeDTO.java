package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonProperty;
import it.gov.pagopa.onboarding.workflow.enums.SelfCriteriaBooleanTypeCode;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


@NoArgsConstructor
@AllArgsConstructor
@Data
public class SelfCriteriaBooleanTypeDTO implements SelfDeclarationItemsDTO {

  @JsonProperty("boolean_type")
  private String type;

  @JsonProperty("description")
  private String description;

  @JsonProperty("subDescription")
  private String subDescription;

  @JsonProperty("value")
  private Boolean value;


  @JsonProperty("code")
  private SelfCriteriaBooleanTypeCode code;

}


