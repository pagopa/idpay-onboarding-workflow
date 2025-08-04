package it.gov.pagopa.onboarding.workflow.controller.web;

import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeAdditionalDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeBeneficiaryRuleDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBooleanTypeDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.ConsentPutWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.enums.SelfCriteriaBooleanTypeCode;
import it.gov.pagopa.onboarding.workflow.exception.custom.EmailNotMatchedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import it.gov.pagopa.onboarding.workflow.exception.custom.PDNDConsentDeniedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.TosNotConfirmedException;
import it.gov.pagopa.onboarding.workflow.service.web.OnboardingServiceWeb;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
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


      // Assert
    assertNotNull(response);
      assertEquals(HttpStatus.OK, response.getStatusCode()); // ✅ nuovo modo
      assertEquals(initiativeWebDTO, response.getBody());
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

    @Test
    void saveConsentWeb_ShouldCallServiceAndReturnAccepted() {
        String userId = "USER123";
        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId("INITIATIVE_1");
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);

        ResponseEntity<Void> response = controller.saveConsentWeb(consent, userId);

        verify(onboardingServiceWeb, times(1)).saveConsentWeb(consent, userId);
        assertEquals(HttpStatus.ACCEPTED.value(), response.getStatusCode().value()); // ✅

    }

    @Test
    void saveConsentWeb_ShouldThrowTosNotConfirmedException_WhenTosFalse() {
        String userId = "USER123";
        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId("INITIATIVE_1");
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(false);
        consent.setPdndAccept(true);

        doThrow(new TosNotConfirmedException("Terms and Conditions not accepted."))
                .when(onboardingServiceWeb).saveConsentWeb(consent, userId);

        assertThrows(TosNotConfirmedException.class, () -> controller.saveConsentWeb(consent, userId));

        verify(onboardingServiceWeb, times(1)).saveConsentWeb(consent, userId);
    }


    @Test
    void saveConsentWeb_ShouldThrowEmailNotMatchedException_WhenEmailsNotMatch() {
        String userId = "USER123";
        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId("INITIATIVE_1");
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("wrong@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);

        doThrow(new EmailNotMatchedException("Email and confirmation email do not match."))
                .when(onboardingServiceWeb).saveConsentWeb(consent, userId);

        assertThrows(EmailNotMatchedException.class, () -> controller.saveConsentWeb(consent, userId));

        verify(onboardingServiceWeb, times(1)).saveConsentWeb(consent, userId);
    }

    @Test
    void saveConsentWeb_ShouldThrowPDNDConsentDeniedException_WhenPdndNotAccepted() {
        String userId = "USER123";
        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId("INITIATIVE_1");
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(false);

        doThrow(new PDNDConsentDeniedException("PDND Consent denied"))
                .when(onboardingServiceWeb).saveConsentWeb(consent, userId);

        assertThrows(PDNDConsentDeniedException.class, () -> controller.saveConsentWeb(consent, userId));

        verify(onboardingServiceWeb, times(1)).saveConsentWeb(consent, userId);
    }
}
