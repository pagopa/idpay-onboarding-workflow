package it.gov.pagopa.onboarding.workflow.dto.mapper;

import it.gov.pagopa.onboarding.workflow.dto.mapper.producerDto.SaveConsentDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.stereotype.Service;

@Service
public class ConsentMapper {

  public SaveConsentDTO map(Onboarding onboarding){

    SaveConsentDTO saveConsentDTO = null;

    if(onboarding!=null){
      saveConsentDTO = SaveConsentDTO.builder().build();
      saveConsentDTO.setInitiativeId(onboarding.getInitiativeId());
      saveConsentDTO.setStatus(onboarding.getStatus());
      saveConsentDTO.setPdndAccept(onboarding.getPdndAccept());
      saveConsentDTO.setSelfDeclarationList(onboarding.getSelfDeclarationList());
      saveConsentDTO.setCriteriaConsensusTimestamp(onboarding.getCriteriaConsensusTimestamp());

    }

    return saveConsentDTO;
  }

}
