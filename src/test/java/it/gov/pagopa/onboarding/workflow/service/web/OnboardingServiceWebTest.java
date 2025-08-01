package it.gov.pagopa.onboarding.workflow.service.web;

import it.gov.pagopa.onboarding.workflow.dto.initiative.*;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.InitiativeWebMapper;
import it.gov.pagopa.onboarding.workflow.enums.SelfCriteriaBooleanTypeCode;
import it.gov.pagopa.onboarding.workflow.service.OnboardingServiceImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class OnboardingServiceWebTest {

  @Mock
  private OnboardingServiceImpl onboardingServiceMock;

  @Mock
  private InitiativeWebMapper initiativeWebMapper;

  @InjectMocks
  private OnboardingServiceWebImpl service;

  private final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private final Locale ACCEPT_LANGUAGE = Locale.ITALIAN;

  private InitiativeDTO initiativeDTO;
  private InitiativeWebDTO initiativeWebDTO;

  @BeforeEach
  void setUp() {
    InitiativeAdditionalDTO additional = new InitiativeAdditionalDTO();
    additional.setPrivacyLink("privacyLink");
    additional.setTcLink("TcLink");

    InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
    beneficiaryRule.setSelfDeclarationCriteria(List.of(
            new SelfCriteriaBooleanTypeDTO("boolean_type", "ciao", "ciao ciao", true, SelfCriteriaBooleanTypeCode.ISEE)
    ));

    initiativeDTO = new InitiativeDTO();
    initiativeDTO.setAdditionalInfo(additional);
    initiativeDTO.setBeneficiaryRule(beneficiaryRule);

    initiativeWebDTO = new InitiativeWebDTO(additional, beneficiaryRule);
  }

  @Test
  void getInitiativeWeb_shouldReturnMappedDto_whenInitiativeExists() {
    when(onboardingServiceMock.getInitiative(INITIATIVE_ID)).thenReturn(initiativeDTO);
    when(initiativeWebMapper.map(initiativeDTO)).thenReturn(initiativeWebDTO);

    InitiativeWebDTO result = service.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE);

    assertNotNull(result);
    assertEquals(initiativeWebDTO, result);

    verify(onboardingServiceMock, times(1)).getInitiative(INITIATIVE_ID);
    verify(initiativeWebMapper, times(1)).map(initiativeDTO);
  }

  @Test
  void getInitiativeWeb_shouldReturnNull_whenInitiativeDoesNotExist() {
    when(onboardingServiceMock.getInitiative(INITIATIVE_ID)).thenReturn(null);

    InitiativeWebDTO result = service.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE);

    assertNull(result);

    verify(onboardingServiceMock, times(1)).getInitiative(INITIATIVE_ID);
    verifyNoInteractions(initiativeWebMapper);
  }
}
