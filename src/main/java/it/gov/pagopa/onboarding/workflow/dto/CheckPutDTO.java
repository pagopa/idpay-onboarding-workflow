package it.gov.pagopa.onboarding.workflow.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CheckPutDTO {

  @NotBlank(message="The field is mandatory")
  String initiativeId;

  @NotBlank (message="The field is mandatory")
  String channel;
}
