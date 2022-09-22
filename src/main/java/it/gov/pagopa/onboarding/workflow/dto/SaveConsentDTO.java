package it.gov.pagopa.onboarding.workflow.dto;

import java.time.LocalDateTime;
import java.util.Map;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class SaveConsentDTO {

  String userId;

  String initiativeId;

  Boolean tc;

  String status;

  Boolean pdndAccept;

  Map<String, Boolean> selfDeclarationBool;

  Map<String, String> selfDeclarationMulti;

  LocalDateTime tcAcceptTimestamp;

  LocalDateTime criteriaConsensusTimestamp;

}
