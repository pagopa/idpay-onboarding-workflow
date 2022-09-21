package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class SelfCriteriaBoolDTO implements SelfDeclarationItemsDTO {

  @JsonProperty("_type")
  private String type;

  @JsonProperty("description")
  private String description;

  @JsonProperty("value")
  private Boolean value;

  @JsonProperty("code")
  private String code;

}
