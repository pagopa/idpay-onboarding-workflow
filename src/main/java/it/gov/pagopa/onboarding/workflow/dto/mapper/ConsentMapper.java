package it.gov.pagopa.onboarding.workflow.dto.mapper;

import it.gov.pagopa.onboarding.workflow.dto.SaveConsentDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.stereotype.Service;

@Service
public class ConsentMapper {

  public SaveConsentDTO map(Onboarding onboarding) {

    return SaveConsentDTO.builder()
        .userId(onboarding.getUserId())
        .initiativeId(onboarding.getInitiativeId())
        .status(onboarding.getStatus())
        .pdndAccept(onboarding.getPdndAccept())
        .selfDeclarationList(onboarding.getSelfDeclarationList())
        .criteriaConsensusTimestamp(onboarding.getCriteriaConsensusTimestamp())
        .tc(onboarding.getTc())
        .tcAcceptTimestamp(onboarding.getTcAcceptTimestamp())
        .build();

  }

}
