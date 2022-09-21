package it.gov.pagopa.onboarding.workflow.dto;

import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfDeclarationItemsDTO;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class SaveConsentDTO {

  String userId;

  String initiativeId;

  boolean tc;

  String status;

  Boolean pdndAccept;

  List<SelfDeclarationItemsDTO> selfDeclarationList;

  LocalDateTime tcAcceptTimestamp;

  LocalDateTime criteriaConsensusTimestamp;

}
