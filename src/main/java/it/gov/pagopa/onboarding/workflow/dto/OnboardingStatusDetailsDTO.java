package it.gov.pagopa.onboarding.workflow.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.time.LocalDateTime;

public record OnboardingStatusDetailsDTO(
    String status,
    LocalDateTime statusDate,
    LocalDateTime onboardingOkDate,
    @JsonProperty("family_id") String familyId
) {}
