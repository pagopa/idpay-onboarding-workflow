package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class SelfCriteriaMultiConsentDTO implements SelfDeclarationItemsDTO {

  @JsonProperty("_type")
  private String type;

  @JsonProperty("description")
  private String description;

  @JsonProperty("value")
  private List<String> value;

  @JsonProperty("code")
  private String code;

  @JsonIgnore
  public String getFirst(){
    return value.get(0);
  }

}
