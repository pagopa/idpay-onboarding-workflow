package it.gov.pagopa.onboarding.workflow.dto.mapper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.SaveConsentDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfDeclarationItemsDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import java.time.LocalDateTime;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = ConsentMapper.class)
 class ConsentMapperTest {
  private static final String USER_ID_OK = "123";
  private static final String INITIATIVE_ID_OK = "123";
  @Autowired
  ConsentMapper consentMapper;

  @Test
  void  map_ok(){
    List<SelfDeclarationItemsDTO> selfConsent = List.of(new SelfCriteriaBoolDTO("boolean", "descr",true, "1"));
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID_OK,USER_ID_OK);
    onboarding.setPdndAccept(true);
    onboarding.setTc(true);
    onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
    onboarding.setCriteriaConsensusTimestamp(LocalDateTime.now());
    onboarding.setSelfDeclarationList(selfConsent);
    SaveConsentDTO saveConsentDTO = consentMapper.map(onboarding);

    assertNotNull(saveConsentDTO);
    assertEquals(onboarding.getUserId(), saveConsentDTO.getUserId());
    assertEquals(onboarding.getStatus(), saveConsentDTO.getStatus());
    assertEquals(onboarding.getInitiativeId(), saveConsentDTO.getInitiativeId());
    assertEquals(onboarding.getTc(), saveConsentDTO.isTc());
    assertEquals(onboarding.getTcAcceptTimestamp(), saveConsentDTO.getTcAcceptTimestamp());
    assertEquals(onboarding.getPdndAccept(), saveConsentDTO.getPdndAccept());
    assertEquals(onboarding.getSelfDeclarationList(), saveConsentDTO.getSelfDeclarationList());
    assertEquals(onboarding.getCriteriaConsensusTimestamp(), saveConsentDTO.getCriteriaConsensusTimestamp());

  }

}
