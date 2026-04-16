package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

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

  Instant tcAcceptTimestamp;

  Instant criteriaConsensusTimestamp;

  //New attribute
  String serviceId;

  String name;

  String surname;

}
