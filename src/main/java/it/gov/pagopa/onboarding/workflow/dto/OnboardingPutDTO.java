package it.gov.pagopa.onboarding.workflow.dto;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingPutDTO {

  @NotBlank (message="Field initiativeId cannot be blank!")
  @NotNull (message="Field initiativeId cannot be null!")
  String initiativeId;

}

