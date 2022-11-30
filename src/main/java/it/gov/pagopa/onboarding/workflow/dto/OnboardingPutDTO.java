package it.gov.pagopa.onboarding.workflow.dto;

import javax.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingPutDTO {

  @NotBlank (message="The field is mandatory!")
  String initiativeId;


}

