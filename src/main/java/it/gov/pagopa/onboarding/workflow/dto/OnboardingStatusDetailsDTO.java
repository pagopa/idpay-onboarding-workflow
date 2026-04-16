package it.gov.pagopa.onboarding.workflow.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.time.Instant;

public record OnboardingStatusDetailsDTO(
    String status,
    Instant statusDate,
    Instant onboardingOkDate,
    @JsonProperty("family_id") String familyId
) {}
