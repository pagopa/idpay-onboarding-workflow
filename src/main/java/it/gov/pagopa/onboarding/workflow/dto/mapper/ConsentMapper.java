package it.gov.pagopa.onboarding.workflow.dto.mapper;

import it.gov.pagopa.onboarding.workflow.dto.mapper.producer.SaveConsentDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.stereotype.Service;

@Service
public class ConsentMapper {

  public SaveConsentDTO map(Onboarding onboarding){

    SaveConsentDTO saveConsentDTO = null;

    if(onboarding!=null){
      saveConsentDTO = SaveConsentDTO.builder().build();
      saveConsentDTO.setUserId(onboarding.getUserId());
      saveConsentDTO.setInitiativeId(onboarding.getInitiativeId());
      saveConsentDTO.setStatus(onboarding.getStatus());
      saveConsentDTO.setPdndAccept(onboarding.getPdndAccept());
      saveConsentDTO.setSelfDeclarationList(onboarding.getSelfDeclarationList());
      saveConsentDTO.setCriteriaConsensusTimestamp(onboarding.getCriteriaConsensusTimestamp());
      saveConsentDTO.setTc(onboarding.isTc());
      saveConsentDTO.setTcAcceptTimestamp(onboarding.getTcAcceptTimestamp());

    }

    return saveConsentDTO;
  }

}
