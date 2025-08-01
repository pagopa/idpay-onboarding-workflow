package it.gov.pagopa.onboarding.workflow.controller.web;

import it.gov.pagopa.onboarding.workflow.dto.web.ConsentPutWebDTO;
import it.gov.pagopa.onboarding.workflow.exception.custom.EmailNotMatchedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.PDNDConsentDeniedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.TosNotConfirmedException;
import it.gov.pagopa.onboarding.workflow.service.web.OnboardingServiceWeb;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.ResponseEntity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

class OnboardingControllerWebTest {

    @InjectMocks
    private OnboardingControllerWebImpl controller;

    @Mock
    private OnboardingServiceWeb onboardingServiceWeb;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
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
        assertEquals(202, response.getStatusCodeValue());
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
