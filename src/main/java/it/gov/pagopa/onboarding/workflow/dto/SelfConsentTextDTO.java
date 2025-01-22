package it.gov.pagopa.onboarding.workflow.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SelfConsentTextDTO implements SelfConsentDTO {

  @JsonProperty("_type")
  String type;

  String code;

  String value;

}

