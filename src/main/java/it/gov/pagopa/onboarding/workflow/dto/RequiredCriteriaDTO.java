package it.gov.pagopa.onboarding.workflow.dto;

import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfDeclarationItemsDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RequiredCriteriaDTO {

  List<PDNDCriteriaDTO> pdndCriteria = null;

  List<SelfDeclarationItemsDTO> selfDeclarationList = null;

}

