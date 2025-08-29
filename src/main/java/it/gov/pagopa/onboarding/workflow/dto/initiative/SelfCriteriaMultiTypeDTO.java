package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;


@NoArgsConstructor
@AllArgsConstructor
@Data
public class SelfCriteriaMultiTypeDTO implements SelfDeclarationItemsDTO {

  @JsonProperty("_type")
  private String type;
  @JsonProperty("description")
  private String description;
  @JsonProperty("subDescription")
  private String subDescription;
  @JsonProperty("value")
  private List<SelfCriteriaMultiTypeValueDTO> value;
  @JsonProperty("code")
  private String code;
}


