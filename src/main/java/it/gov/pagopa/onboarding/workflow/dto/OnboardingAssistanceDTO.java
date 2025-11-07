package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OnboardingAssistanceDTO {

    String userId;
    String initiativeId;
    String status;
    String userMail;
    String channel;
    String name;
    String detailKO;
    String surname;
    Boolean tc;
    Boolean pdndAccept;
    LocalDateTime tcAcceptTimestamp;
    LocalDateTime criteriaConsensusTimestamp;
    LocalDateTime onboardingOkDate;
}
