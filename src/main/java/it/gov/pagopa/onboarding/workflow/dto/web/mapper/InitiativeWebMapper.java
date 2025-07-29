package it.gov.pagopa.onboarding.workflow.dto.web.mapper;

import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import org.springframework.stereotype.Service;

@Service
public class InitiativeWebMapper {

  public InitiativeWebDTO map(InitiativeDTO initiativeDTO) {

    return InitiativeWebDTO.builder()
        .general(initiativeDTO.getGeneral())
        .beneficiaryRule(initiativeDTO.getBeneficiaryRule())
        .build();

  }

}
