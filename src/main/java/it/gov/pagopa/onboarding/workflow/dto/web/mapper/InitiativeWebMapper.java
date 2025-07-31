package it.gov.pagopa.onboarding.workflow.dto.web.mapper;

import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import org.springframework.stereotype.Service;

@Service
public class InitiativeWebMapper {

  public InitiativeWebDTO map(InitiativeDTO initiativeDTO, InitiativeGeneralWebDTO initiativeGeneralWebDTO) {


    return InitiativeWebDTO.builder()
        .generalWeb(initiativeGeneralWebDTO)
        .beneficiaryRule(initiativeDTO.getBeneficiaryRule())
        .build();

  }

}
