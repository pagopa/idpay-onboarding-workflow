package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingStatusDTO {

    /*
    public enum StatusEnum {

        ACCEPTED_TC("ACCEPTED_TC"),

        ON_EVALUATION("ON_EVALUATION"),

        ONBOARDING_KO("ONBOARDING_KO"),

        REGISTERED_ONLY_IBAN("REGISTERED_ONLY_IBAN"),

        REGISTERED_ONLY_CC("REGISTERED_ONLY_CC"),

        REGISTERED_REFUNDABLE("REGISTERED_REFUNDABLE");

        private String value;

        StatusEnum(String value) {
            this.value = value;
        }

    }*/

  private String status;

}

