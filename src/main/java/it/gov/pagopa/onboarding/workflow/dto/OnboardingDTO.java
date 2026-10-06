package it.gov.pagopa.onboarding.workflow.dto;

import it.gov.pagopa.onboarding.workflow.dto.initiative.VerifyDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OnboardingDTO {

  String userId;

  String initiativeId;

  Boolean tc;

  List<VerifyDTO> verifies;

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
