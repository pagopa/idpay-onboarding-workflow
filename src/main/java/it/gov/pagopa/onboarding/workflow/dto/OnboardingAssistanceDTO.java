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

    private String userId;
    private String initiativeId;
    private String status;
    private String userMail;
    private String channel;
    private String name;
    private String detailKO;
    private String surname;
    private Boolean tc;
    private Boolean pdndAccept;
    private LocalDateTime tcAcceptTimestamp;
    private LocalDateTime criteriaConsensusTimestamp;
    private LocalDateTime onboardingOkDate;
}
