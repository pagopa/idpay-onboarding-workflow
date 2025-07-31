package it.gov.pagopa.onboarding.workflow.dto.web;

import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class OnBoardingWebDTO extends OnboardingDTO {

    private String channel;

}
