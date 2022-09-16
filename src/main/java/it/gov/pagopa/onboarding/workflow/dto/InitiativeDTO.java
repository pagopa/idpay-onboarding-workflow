package it.gov.pagopa.onboarding.workflow.dto;

import java.util.ArrayList;
import java.util.List;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class InitiativeDTO {

  List<PDNDCriteriaDTO> pdndCriteria;
  List<SelfDeclarationDTO> selfDeclarationList;

  public InitiativeDTO() {
    List<PDNDCriteriaDTO> pdnd = new ArrayList<>();
    PDNDCriteriaDTO pdndCriteriaDTO1 = new PDNDCriteriaDTO("01", "test_descrizione_pdnd",
        "test_ente_pdnd");
    PDNDCriteriaDTO pdndCriteriaDTO2 = new PDNDCriteriaDTO("02", "test_descrizione_pdnd",
        "test_ente_pdnd");
    pdnd.add(pdndCriteriaDTO1);
    pdnd.add(pdndCriteriaDTO2);
    this.pdndCriteria = pdnd;
    List<SelfDeclarationDTO> selfDeclaration = new ArrayList<>();
    SelfDeclarationDTO selfDeclarationDTO1 = new SelfDeclarationDTO("1",
        "test_descrizione_selfDeclaration");
    SelfDeclarationDTO selfDeclarationDTO2 = new SelfDeclarationDTO("2",
        "test_descrizione_selfDeclaration");
    selfDeclaration.add(selfDeclarationDTO1);
    selfDeclaration.add(selfDeclarationDTO2);
    this.selfDeclarationList = selfDeclaration;
  }


  public InitiativeDTO(List<PDNDCriteriaDTO> pdndCriteria,
      List<SelfDeclarationDTO> selfDeclarationList) {
    this.pdndCriteria = pdndCriteria;
    this.selfDeclarationList = selfDeclarationList;
  }
}
