package it.gov.pagopa.onboarding.workflow.dto;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsentPutDTO {

  String initiativeId = null;

  Boolean pdndAccept = null;

  List<SelfConsentDTO> selfDeclarationList = null;

}

