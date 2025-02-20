package it.gov.pagopa.onboarding.workflow.dto;

import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class OnboardingDTO {

  String userId;

  String initiativeId;

  Boolean tc;

  String status;

  Boolean pdndAccept;

  LocalDateTime tcAcceptTimestamp;

  LocalDateTime criteriaConsensusTimestamp;

  //New attribute
  String serviceId;

}
