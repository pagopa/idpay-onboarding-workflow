//package it.gov.pagopa.onboarding.workflow.controller.web;
//
//import it.gov.pagopa.onboarding.workflow.dto.ConsentPutUnifiedDTO;
//import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeAdditionalDTO;
//import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeBeneficiaryRuleDTO;
//import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeGeneralDTO;
//import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBooleanTypeDTO;
//import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
//import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
//import it.gov.pagopa.onboarding.workflow.dto.web.mapper.GeneralWebMapper;
//import it.gov.pagopa.onboarding.workflow.enums.ChannelType;
//import it.gov.pagopa.onboarding.workflow.enums.SelfCriteriaBooleanTypeCode;
//import it.gov.pagopa.onboarding.workflow.exception.custom.EmailNotMatchedException;
//import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
//import it.gov.pagopa.onboarding.workflow.exception.custom.PDNDConsentDeniedException;
//import it.gov.pagopa.onboarding.workflow.exception.custom.TosNotConfirmedException;
//import it.gov.pagopa.onboarding.workflow.service.web.OnboardingServiceWeb;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.mockito.InjectMocks;
//import org.mockito.Mock;
//import org.mockito.junit.jupiter.MockitoExtension;
//import org.springframework.http.HttpStatus;
//import org.springframework.http.ResponseEntity;
//
//import java.time.LocalDate;
//import java.util.HashMap;
//import java.util.List;
//import java.util.Locale;
//import java.util.Map;
//
//import static org.junit.jupiter.api.Assertions.*;
//import static org.mockito.Mockito.*;
//
//@ExtendWith(MockitoExtension.class)
//class OnboardingControllerWebTest {
//
//  @Mock
//  private OnboardingServiceWeb onboardingServiceWeb;
//
//  @InjectMocks
//  private OnboardingControllerWebImpl controller;
//
//  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
//  private static final Locale ACCEPT_LANGUAGE = Locale.ITALIAN;
//
//  private InitiativeWebDTO initiativeWebDTO;
//
//  @BeforeEach
//  void setUp() {
//    InitiativeAdditionalDTO additionalDTO = new InitiativeAdditionalDTO();
//    additionalDTO.setPrivacyLink("privacyLink");
//    additionalDTO.setTcLink("TcLink");
//
//    InitiativeBeneficiaryRuleDTO beneficiaryRuleDTO = new InitiativeBeneficiaryRuleDTO();
//    beneficiaryRuleDTO.setSelfDeclarationCriteria(
//            List.of(new SelfCriteriaBooleanTypeDTO(
//                    "boolean_type",
//                    "test description",
//                    "test sub description",
//                    true,
//                    SelfCriteriaBooleanTypeCode.ISEE
//            ))
//    );
//
//    GeneralWebMapper generalWebMapper = new GeneralWebMapper();
//
//    InitiativeGeneralDTO initiativeGeneralDTO = new InitiativeGeneralDTO();
//    initiativeGeneralDTO.setStartDate(LocalDate.MIN);
//    initiativeGeneralDTO.setEndDate(LocalDate.MAX);
//    Map<String, String> language = new HashMap<>();
//    language.put(Locale.ITALIAN.getLanguage(), "it");
//    initiativeGeneralDTO.setDescriptionMap(language);
//
//    InitiativeGeneralWebDTO initiativeGeneralWebDTO = generalWebMapper.map(initiativeGeneralDTO, ACCEPT_LANGUAGE);
//    initiativeWebDTO = new InitiativeWebDTO(initiativeGeneralWebDTO, additionalDTO, beneficiaryRuleDTO);
//  }
//
//  @Test
//  void getInitiativeWeb_ok() {
//    when(onboardingServiceWeb.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE))
//            .thenReturn(initiativeWebDTO);
//
//    ResponseEntity<InitiativeWebDTO> response =
//            controller.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE);
//
//    assertNotNull(response);
//
//    assertEquals(HttpStatus.OK, response.getStatusCode());
//    assertEquals(initiativeWebDTO, response.getBody());
//    verify(onboardingServiceWeb).getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE);
//    verifyNoMoreInteractions(onboardingServiceWeb);
//  }
//
//  @Test
//  void getInitiativeWeb_ko() {
//    when(onboardingServiceWeb.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE))
//            .thenReturn(null);
//
//    InitiativeNotFoundException exception = assertThrows(
//            InitiativeNotFoundException.class,
//            () -> controller.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE)
//    );
//
//    assertTrue(exception.getMessage().contains(INITIATIVE_ID));
//    verify(onboardingServiceWeb).getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE);
//    verifyNoMoreInteractions(onboardingServiceWeb);
//  }
//
//  @Test
//  void saveConsentUnifiedWeb_ShouldCallServiceAndReturnAccepted() {
//    String userId = "USER123";
//    ChannelType channel = ChannelType.WEB;
//
//    ConsentPutUnifiedDTO consent = new ConsentPutUnifiedDTO();
//    consent.setInitiativeId("INITIATIVE_1");
//    consent.setUserMail("test@mail.com");
//    consent.setUserMailConfirmation("test@mail.com");
//    consent.setConfirmedTos(true);
//    consent.setPdndAccept(true);
//
//    ResponseEntity<Void> response = controller.saveConsentUnified(consent, channel, userId);
//
//    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
//    verify(onboardingServiceWeb, times(1)).saveConsentUnified(consent, userId);
//  }
//
//  @Test
//  void saveConsentUnifiedAppIo_ShouldCallServiceAndReturnAccepted() {
//    String userId = "USER123";
//    ChannelType channel = ChannelType.APP_IO;
//
//    ConsentPutUnifiedDTO consent = new ConsentPutUnifiedDTO();
//    consent.setInitiativeId("INITIATIVE_1");
//    consent.setConfirmedTos(true);
//    consent.setPdndAccept(true);
//
//    ResponseEntity<Void> response = controller.saveConsentUnified(consent, channel, userId);
//
//    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
//    verify(onboardingServiceWeb, times(1)).saveConsentUnified(consent, userId);
//  }
//
//
//  @Test
//  void saveConsentUnifiedWeb_ShouldThrowTosNotConfirmedException_WhenTosFalse() {
//    String userId = "USER123";
//    ChannelType channel = ChannelType.WEB;
//
//    ConsentPutUnifiedDTO consent = new ConsentPutUnifiedDTO();
//    consent.setInitiativeId("INITIATIVE_1");
//    consent.setUserMail("test@mail.com");
//    consent.setUserMailConfirmation("test@mail.com");
//    consent.setConfirmedTos(false);
//    consent.setPdndAccept(true);
//
//    doThrow(new TosNotConfirmedException("Terms and Conditions not accepted."))
//            .when(onboardingServiceWeb).saveConsentUnified(consent, userId);
//
//    assertThrows(TosNotConfirmedException.class, () -> controller.saveConsentUnified(consent, channel, userId));
//
//    verify(onboardingServiceWeb, times(1)).saveConsentUnified(consent, userId);
//  }
//
//  @Test
//  void saveConsentUnifiedAppIo_ShouldThrowTosNotConfirmedException_WhenTosIsFalse() {
//    String userId = "USER789";
//    ChannelType channel = ChannelType.APP_IO;
//
//    ConsentPutUnifiedDTO consent = new ConsentPutUnifiedDTO();
//    consent.setInitiativeId("INITIATIVE_2");
//    consent.setConfirmedTos(false);
//    consent.setPdndAccept(true);
//
//    doThrow(new TosNotConfirmedException("Terms and Conditions not accepted."))
//            .when(onboardingServiceWeb).saveConsentUnified(consent, userId);
//
//    assertThrows(TosNotConfirmedException.class, () ->
//            controller.saveConsentUnified(consent, channel, userId)
//    );
//
//    verify(onboardingServiceWeb).saveConsentUnified(consent, userId);
//  }
//
//
//  @Test
//  void saveConsentUnifiedWeb_ShouldThrowEmailNotMatchedException_WhenEmailsNotMatch() {
//    String userId = "USER123";
//    ChannelType channel = ChannelType.WEB;
//
//    ConsentPutUnifiedDTO consent = new ConsentPutUnifiedDTO();
//    consent.setInitiativeId("INITIATIVE_1");
//    consent.setUserMail("test@mail.com");
//    consent.setUserMailConfirmation("wrong@mail.com");
//    consent.setConfirmedTos(true);
//    consent.setPdndAccept(true);
//
//    doThrow(new EmailNotMatchedException("Email and confirmation email do not match."))
//            .when(onboardingServiceWeb).saveConsentUnified(consent, userId);
//
//    assertThrows(EmailNotMatchedException.class, () -> controller.saveConsentUnified(consent, channel, userId));
//
//    verify(onboardingServiceWeb, times(1)).saveConsentUnified(consent, userId);
//  }
//
//
//  @Test
//  void saveConsentUnifiedWeb_ShouldThrowPDNDConsentDeniedException_WhenPdndNotAccepted() {
//    String userId = "USER123";
//    ChannelType channel = ChannelType.WEB;
//
//    ConsentPutUnifiedDTO consent = new ConsentPutUnifiedDTO();
//    consent.setInitiativeId("INITIATIVE_1");
//    consent.setUserMail("test@mail.com");
//    consent.setUserMailConfirmation("test@mail.com");
//    consent.setConfirmedTos(true);
//    consent.setPdndAccept(false);
//
//    doThrow(new PDNDConsentDeniedException("PDND Consent denied"))
//            .when(onboardingServiceWeb).saveConsentUnified(consent, userId);
//
//    assertThrows(PDNDConsentDeniedException.class, () -> controller.saveConsentUnified(consent, channel, userId));
//
//    verify(onboardingServiceWeb, times(1)).saveConsentUnified(consent, userId);
//  }
//
//  @Test
//  void saveConsentUnifiedAppIo_ShouldThrowPDNDConsentDeniedException_WhenPdndNotAccepted() {
//    String userId = "USER987";
//    ChannelType channel = ChannelType.APP_IO;
//
//    ConsentPutUnifiedDTO consent = new ConsentPutUnifiedDTO();
//    consent.setInitiativeId("INITIATIVE_3");
//    consent.setConfirmedTos(true);
//    consent.setPdndAccept(false);
//
//    doThrow(new PDNDConsentDeniedException("PDND Consent denied"))
//            .when(onboardingServiceWeb).saveConsentUnified(consent, userId);
//
//    assertThrows(PDNDConsentDeniedException.class, () ->
//            controller.saveConsentUnified(consent, channel, userId)
//    );
//
//    verify(onboardingServiceWeb).saveConsentUnified(consent, userId);
//  }
//
//
//}
