package it.gov.pagopa.onboarding.workflow.controller.web;

import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeAdditionalDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeBeneficiaryRuleDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBooleanTypeDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.enums.SelfCriteriaBooleanTypeCode;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import it.gov.pagopa.onboarding.workflow.service.web.OnboardingServiceWeb;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class OnboardingControllerWebTest {

  @Mock
  private OnboardingServiceWeb onboardingServiceWeb;

  @InjectMocks
  private OnboardingControllerWebImpl controller;

  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private static final Locale ACCEPT_LANGUAGE = Locale.ITALIAN;

  private InitiativeWebDTO initiativeWebDTO;

  @BeforeEach
  void setUp() {
    InitiativeAdditionalDTO additionalDTO = new InitiativeAdditionalDTO();
    additionalDTO.setPrivacyLink("privacyLink");
    additionalDTO.setTcLink("TcLink");

    InitiativeBeneficiaryRuleDTO beneficiaryRuleDTO = new InitiativeBeneficiaryRuleDTO();
    beneficiaryRuleDTO.setSelfDeclarationCriteria(
            List.of(new SelfCriteriaBooleanTypeDTO(
                    "boolean_type",
                    "ciao",
                    "ciao ciao",
                    true,
                    SelfCriteriaBooleanTypeCode.ISEE
            ))
    );

    initiativeWebDTO = new InitiativeWebDTO(additionalDTO, beneficiaryRuleDTO);
  }

  @Test
  void getInitiativeWeb_ok() {
    // Arrange
    when(onboardingServiceWeb.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE))
            .thenReturn(initiativeWebDTO);

    // Act
    ResponseEntity<InitiativeWebDTO> response =
            controller.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE);

    // Assert
    assertNotNull(response);
    assertEquals(200, response.getStatusCodeValue());
    assertSame(initiativeWebDTO, response.getBody());
    verify(onboardingServiceWeb).getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE);
    verifyNoMoreInteractions(onboardingServiceWeb);
  }

  @Test
  void getInitiativeWeb_ko() {
    // Arrange
    when(onboardingServiceWeb.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE))
            .thenReturn(null);

    // Act & Assert
    InitiativeNotFoundException exception = assertThrows(
            InitiativeNotFoundException.class,
            () -> controller.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE)
    );

    assertTrue(exception.getMessage().contains(INITIATIVE_ID));
    verify(onboardingServiceWeb).getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE);
    verifyNoMoreInteractions(onboardingServiceWeb);
  }
}
