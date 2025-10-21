package it.gov.pagopa.onboarding.workflow.dto.mapper;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfDeclarationItemsDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = ConsentMapper.class)
class ConsentMapperTest {

  private static final String USER_ID_OK = "USER_ID";
  private static final String INITIATIVE_ID_OK = "INITIATIVE_ID";
  @Autowired
  ConsentMapper consentMapper;

  @Test
  void map_ok() {
    List<SelfDeclarationItemsDTO> selfConsent = List.of(
        new SelfCriteriaBoolDTO("boolean", "descr", "subDescr", true, "1"),
        new SelfCriteriaMultiDTO("multi", "descr", "subDescr", List.of("Value"), "2"));
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID_OK, USER_ID_OK);
    onboarding.setPdndAccept(true);
    onboarding.setTc(true);
    onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
    onboarding.setCriteriaConsensusTimestamp(LocalDateTime.now());
    OnboardingDTO onboardingDTO = consentMapper.map(onboarding);

    assertNotNull(onboardingDTO);
    assertEquals(onboarding.getUserId(), onboardingDTO.getUserId());
    assertEquals(onboarding.getStatus(), onboardingDTO.getStatus());
    assertEquals(onboarding.getInitiativeId(), onboardingDTO.getInitiativeId());
    assertEquals(onboarding.getTc(), onboardingDTO.getTc());
    assertEquals(onboarding.getTcAcceptTimestamp(), onboardingDTO.getTcAcceptTimestamp());
    assertEquals(onboarding.getPdndAccept(), onboardingDTO.getPdndAccept());
    assertEquals(onboarding.getCriteriaConsensusTimestamp(),
        onboardingDTO.getCriteriaConsensusTimestamp());

  }

}
