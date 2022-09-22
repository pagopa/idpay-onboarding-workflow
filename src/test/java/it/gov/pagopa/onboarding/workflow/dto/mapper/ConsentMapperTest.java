package it.gov.pagopa.onboarding.workflow.dto.mapper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfDeclarationItemsDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = ConsentMapper.class)
class ConsentMapperTest {

  private static final String USER_ID_OK = "USER_ID";
  private static final String INITIATIVE_ID_OK = "INITIATIVE_ID";
  private static final Map<String, Boolean> SELF_BOOL = new HashMap<>();
  private static final Map<String, String> SELF_MULTI = new HashMap<>();
  @Autowired
  ConsentMapper consentMapper;

  static {
    SELF_BOOL.put("1", true);
    SELF_MULTI.put("2", "Value");
  }

  @Test
  void map_ok() {
    List<SelfDeclarationItemsDTO> selfConsent = List.of(
        new SelfCriteriaBoolDTO("boolean", "descr", true, "1"),
        new SelfCriteriaMultiDTO("multi", "descr", List.of("Value"), "2"));
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID_OK, USER_ID_OK);
    onboarding.setPdndAccept(true);
    onboarding.setTc(true);
    onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
    onboarding.setCriteriaConsensusTimestamp(LocalDateTime.now());
    onboarding.setSelfDeclarationList(selfConsent);
    OnboardingDTO onboardingDTO = consentMapper.map(onboarding);

    assertNotNull(onboardingDTO);
    assertEquals(onboarding.getUserId(), onboardingDTO.getUserId());
    assertEquals(onboarding.getStatus(), onboardingDTO.getStatus());
    assertEquals(onboarding.getInitiativeId(), onboardingDTO.getInitiativeId());
    assertEquals(onboarding.getTc(), onboardingDTO.getTc());
    assertEquals(onboarding.getTcAcceptTimestamp(), onboardingDTO.getTcAcceptTimestamp());
    assertEquals(onboarding.getPdndAccept(), onboardingDTO.getPdndAccept());
    assertEquals(SELF_BOOL, onboardingDTO.getSelfDeclarationBool());
    assertEquals(SELF_MULTI, onboardingDTO.getSelfDeclarationMulti());
    assertEquals(onboarding.getCriteriaConsensusTimestamp(),
        onboardingDTO.getCriteriaConsensusTimestamp());

  }

}
