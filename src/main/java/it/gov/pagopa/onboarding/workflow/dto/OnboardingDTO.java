package it.gov.pagopa.onboarding.workflow.dto;

import java.time.LocalDateTime;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingDTO {

  String userId;

  String initiativeId;

  Boolean tc;

  String status;

  Boolean pdndAccept;

  String userMail;

  LocalDateTime tcAcceptTimestamp;

  LocalDateTime criteriaConsensusTimestamp;

  //New attribute
  String serviceId;

}
