package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingDTO {

  String userId;

  String initiativeId;

  Boolean tc;

  Boolean verifyIsee;

  String status;

  Boolean pdndAccept;

  String userMail;

  String channel;

  LocalDateTime tcAcceptTimestamp;

  LocalDateTime criteriaConsensusTimestamp;

  //New attribute
  String serviceId;

  String name;

  String surname;

}
