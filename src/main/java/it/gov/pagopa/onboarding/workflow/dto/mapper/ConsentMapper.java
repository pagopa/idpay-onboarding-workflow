package it.gov.pagopa.onboarding.workflow.dto.mapper;

import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.stereotype.Service;

@Service
public class ConsentMapper {

  public OnboardingDTO map(Onboarding onboarding) {

    return OnboardingDTO.builder()
        .userId(onboarding.getUserId())
        .initiativeId(onboarding.getInitiativeId())
        .status(onboarding.getStatus())
        .pdndAccept(onboarding.getPdndAccept())
        .criteriaConsensusTimestamp(onboarding.getCriteriaConsensusTimestamp())
        .tc(onboarding.getTc())
        .tcAcceptTimestamp(onboarding.getTcAcceptTimestamp())
        .userMail(onboarding.getUserMail())
        .channel(onboarding.getChannel())
        .build();

  }

}
