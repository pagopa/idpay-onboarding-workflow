package it.gov.pagopa.onboarding.workflow.dto.mapper.producer;

import java.time.LocalDateTime;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SaveConsentDTO {

  String userId;

  String initiativeId;

  boolean tc;

  String status;

  Boolean pdndAccept;

  Map<String, Boolean> selfDeclarationList;

  LocalDateTime tcAcceptTimestamp;

  LocalDateTime criteriaConsensusTimestamp;

}
