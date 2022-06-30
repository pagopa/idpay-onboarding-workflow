package it.gov.pagopa.onboarding.workflow.dto.mapper.producerDto;

import java.time.LocalDateTime;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SaveConsentDTO {

  String initiativeId;

  String status;

  Boolean pdndAccept;

  List<Boolean> selfDeclarationList;

  LocalDateTime criteriaConsensusTimestamp;

}
