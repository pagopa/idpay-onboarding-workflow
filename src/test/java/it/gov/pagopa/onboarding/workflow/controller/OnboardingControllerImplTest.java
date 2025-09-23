package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.*;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.GeneralWebMapper;
import it.gov.pagopa.onboarding.workflow.exception.custom.EmailNotMatchedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import it.gov.pagopa.onboarding.workflow.exception.custom.PDNDConsentDeniedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.TosNotConfirmedException;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static it.gov.pagopa.onboarding.workflow.enums.SelfCriteriaMultiTypeCode.ISEE;
import static java.time.LocalDate.MAX;
import static java.time.LocalDate.MIN;
import static java.util.Locale.ITALIAN;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.springframework.http.HttpStatus.ACCEPTED;
import static org.springframework.http.HttpStatus.OK;

@ExtendWith(MockitoExtension.class)
class OnboardingControllerImplTest {

  @Mock
  private OnboardingService onboardingService;

  @InjectMocks
  private OnboardingControllerImpl controller;

  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private static final Locale ACCEPT_LANGUAGE = ITALIAN;

  private InitiativeWebDTO initiativeWebDTO;

  @BeforeEach
  void setUp() {
    InitiativeAdditionalDTO additionalDTO = new InitiativeAdditionalDTO();
    additionalDTO.setPrivacyLink("privacyLink");
    additionalDTO.setTcLink("TcLink");

    InitiativeBeneficiaryRuleDTO beneficiaryRuleDTO = new InitiativeBeneficiaryRuleDTO();
    beneficiaryRuleDTO.setSelfDeclarationCriteria(
            List.of(new SelfCriteriaMultiTypeDTO(
                    "multi_type",
                    "test description",
                    "test sub description",
                    List.of(new SelfCriteriaMultiTypeValueDTO(
                        "description",
                            "subdescription"
                    )),
                    ISEE.getDescription()
            ))
    );

    GeneralWebMapper generalWebMapper = new GeneralWebMapper();

    InitiativeGeneralDTO initiativeGeneralDTO = new InitiativeGeneralDTO();
    initiativeGeneralDTO.setStartDate(MIN);
    initiativeGeneralDTO.setEndDate(MAX);
    Map<String, String> language = new HashMap<>();
    language.put(ITALIAN.getLanguage(), "it");
    initiativeGeneralDTO.setDescriptionMap(language);
    initiativeGeneralDTO.setFamilyUnitComposition("ANPR");
    InitiativeGeneralWebDTO initiativeGeneralWebDTO = generalWebMapper.map(initiativeGeneralDTO, ACCEPT_LANGUAGE);
    initiativeWebDTO = new InitiativeWebDTO(additionalDTO, beneficiaryRuleDTO, initiativeGeneralWebDTO);
  }

  @Test
  void getInitiativeWeb_ok() {
    when(onboardingService.initiativeDetail(INITIATIVE_ID, ACCEPT_LANGUAGE))
            .thenReturn(initiativeWebDTO);

    ResponseEntity<InitiativeWebDTO> response =
            controller.getInitiativeDetail(INITIATIVE_ID, ACCEPT_LANGUAGE);

    assertNotNull(response);

    assertEquals(OK, response.getStatusCode());
    assertEquals(initiativeWebDTO, response.getBody());
    verify(onboardingService).initiativeDetail(INITIATIVE_ID, ACCEPT_LANGUAGE);
    verifyNoMoreInteractions(onboardingService);
  }

  @Test
  void getInitiativeWeb_ko() {
    when(onboardingService.initiativeDetail(INITIATIVE_ID, ACCEPT_LANGUAGE))
            .thenReturn(null);

    InitiativeNotFoundException exception = assertThrows(
            InitiativeNotFoundException.class,
            () -> controller.getInitiativeDetail(INITIATIVE_ID, ACCEPT_LANGUAGE)
    );

    assertTrue(exception.getMessage().contains(INITIATIVE_ID));
    verify(onboardingService).initiativeDetail(INITIATIVE_ID, ACCEPT_LANGUAGE);
    verifyNoMoreInteractions(onboardingService);
  }

  @Test
  void saveConsentUnifiedWeb_ShouldCallServiceAndReturnAccepted() {
    String userId = "USER123";
    String channel = "CHANNEL";

    ConsentPutDTO consent = new ConsentPutDTO();
    consent.setInitiativeId("INITIATIVE_1");
    consent.setUserMail("test@mail.com");
    consent.setUserMailConfirmation("test@mail.com");
    consent.setConfirmedTos(true);
    consent.setPdndAccept(true);

    ResponseEntity<Void> response = controller.saveOnboarding(consent, channel, userId);

    assertEquals(ACCEPTED, response.getStatusCode());
    verify(onboardingService, times(1)).saveOnboarding(consent, channel, userId);
  }

  @Test
  void saveConsentUnifiedAppIo_ShouldCallServiceAndReturnAccepted() {
    String userId = "USER123";
    String channel = "CHANNEL";


    ConsentPutDTO consent = new ConsentPutDTO();
    consent.setInitiativeId("INITIATIVE_1");
    consent.setConfirmedTos(true);
    consent.setPdndAccept(true);

    ResponseEntity<Void> response = controller.saveOnboarding(consent, channel, userId);

    assertEquals(ACCEPTED, response.getStatusCode());
    verify(onboardingService, times(1)).saveOnboarding(consent, channel, userId);
  }


  @Test
  void saveConsentUnifiedWeb_ShouldThrowTosNotConfirmedException_WhenTosFalse() {
    String userId = "USER123";
    String channel = "CHANNEL";


    ConsentPutDTO consent = new ConsentPutDTO();
    consent.setInitiativeId("INITIATIVE_1");
    consent.setUserMail("test@mail.com");
    consent.setUserMailConfirmation("test@mail.com");
    consent.setConfirmedTos(false);
    consent.setPdndAccept(true);

    doThrow(new TosNotConfirmedException("Terms and Conditions not accepted."))
            .when(onboardingService).saveOnboarding(consent, channel, userId);

    assertThrows(TosNotConfirmedException.class, () -> controller.saveOnboarding(consent, channel, userId));

    verify(onboardingService, times(1)).saveOnboarding(consent, channel, userId);
  }

  @Test
  void saveConsentUnifiedAppIo_ShouldThrowTosNotConfirmedException_WhenTosIsFalse() {
    String userId = "USER789";
    String channel = "CHANNEL";


    ConsentPutDTO consent = new ConsentPutDTO();
    consent.setInitiativeId("INITIATIVE_2");
    consent.setConfirmedTos(false);
    consent.setPdndAccept(true);

    doThrow(new TosNotConfirmedException("Terms and Conditions not accepted."))
            .when(onboardingService).saveOnboarding(consent, channel, userId);

    assertThrows(TosNotConfirmedException.class, () ->
            controller.saveOnboarding(consent, channel, userId)
    );

    verify(onboardingService).saveOnboarding(consent, channel, userId);
  }


  @Test
  void saveConsentUnifiedWeb_ShouldThrowEmailNotMatchedException_WhenEmailsNotMatch() {
    String userId = "USER123";
    String channel = "CHANNEL";


    ConsentPutDTO consent = new ConsentPutDTO();
    consent.setInitiativeId("INITIATIVE_1");
    consent.setUserMail("test@mail.com");
    consent.setUserMailConfirmation("wrong@mail.com");
    consent.setConfirmedTos(true);
    consent.setPdndAccept(true);

    doThrow(new EmailNotMatchedException("Email and confirmation email do not match."))
            .when(onboardingService).saveOnboarding(consent, channel, userId);

    assertThrows(EmailNotMatchedException.class, () -> controller.saveOnboarding(consent, channel, userId));

    verify(onboardingService, times(1)).saveOnboarding(consent, channel, userId);
  }


  @Test
  void saveConsentUnifiedWeb_ShouldThrowPDNDConsentDeniedException_WhenPdndNotAccepted() {
    String userId = "USER123";
    String channel = "CHANNEL";


    ConsentPutDTO consent = new ConsentPutDTO();
    consent.setInitiativeId("INITIATIVE_1");
    consent.setUserMail("test@mail.com");
    consent.setUserMailConfirmation("test@mail.com");
    consent.setConfirmedTos(true);
    consent.setPdndAccept(false);

    doThrow(new PDNDConsentDeniedException("PDND Consent denied"))
            .when(onboardingService).saveOnboarding(consent, channel, userId);

    assertThrows(PDNDConsentDeniedException.class, () -> controller.saveOnboarding(consent, channel, userId));

    verify(onboardingService, times(1)).saveOnboarding(consent, channel, userId);
  }

  @Test
  void saveConsentUnifiedAppIo_ShouldThrowPDNDConsentDeniedException_WhenPdndNotAccepted() {
    String userId = "USER987";
    String channel = "CHANNEL";


    ConsentPutDTO consent = new ConsentPutDTO();
    consent.setInitiativeId("INITIATIVE_3");
    consent.setConfirmedTos(true);
    consent.setPdndAccept(false);

    doThrow(new PDNDConsentDeniedException("PDND Consent denied"))
            .when(onboardingService).saveOnboarding(consent, channel, userId);

    assertThrows(PDNDConsentDeniedException.class, () ->
            controller.saveOnboarding(consent, channel, userId)
    );

    verify(onboardingService).saveOnboarding(consent, channel, userId);
  }
}
