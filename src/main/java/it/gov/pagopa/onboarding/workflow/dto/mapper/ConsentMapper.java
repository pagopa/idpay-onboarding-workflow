package it.gov.pagopa.onboarding.workflow.dto.mapper;

import it.gov.pagopa.onboarding.workflow.dto.SaveConsentDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaMultiDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import java.util.stream.Collectors;
import org.springframework.stereotype.Service;

@Service
public class ConsentMapper {

  public SaveConsentDTO map(Onboarding onboarding) {

    return SaveConsentDTO.builder()
        .userId(onboarding.getUserId())
        .initiativeId(onboarding.getInitiativeId())
        .status(onboarding.getStatus())
        .pdndAccept(onboarding.getPdndAccept())
        .selfDeclarationBool(onboarding.getSelfDeclarationList().stream().filter(item -> item.getClass().equals(
            SelfCriteriaBoolDTO.class)).map(SelfCriteriaBoolDTO.class::cast).collect(
            Collectors.toMap(SelfCriteriaBoolDTO::getCode, SelfCriteriaBoolDTO::getValue)))
        .selfDeclarationMulti(onboarding.getSelfDeclarationList().stream().filter(item -> item.getClass().equals(
            SelfCriteriaMultiDTO.class)).map(SelfCriteriaMultiDTO.class::cast).collect(
            Collectors.toMap(SelfCriteriaMultiDTO::getCode, SelfCriteriaMultiDTO::getFirst)))
        .criteriaConsensusTimestamp(onboarding.getCriteriaConsensusTimestamp())
        .tc(onboarding.getTc())
        .tcAcceptTimestamp(onboarding.getTcAcceptTimestamp())
        .build();

  }

}
