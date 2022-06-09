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
public class RequiredCriteriaDTO {

  List<PDNDCriteriaDTO> pdndCriteria = null;

  List<SelfDeclarationDTO> selfDeclarationList = null;

}

