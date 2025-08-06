package it.gov.pagopa.onboarding.workflow.service.web;

import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.*;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.ConsentPutWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.GeneralWebMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.InitiativeWebMapper;
import it.gov.pagopa.onboarding.workflow.enums.SelfCriteriaBooleanTypeCode;
import it.gov.pagopa.onboarding.workflow.event.producer.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.exception.custom.EmailNotMatchedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.PDNDConsentDeniedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.TosNotConfirmedException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import it.gov.pagopa.onboarding.workflow.repository.SelfDeclarationRepository;
import it.gov.pagopa.onboarding.workflow.utils.AuditUtilities;
import it.gov.pagopa.onboarding.workflow.utils.Utilities;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.PDND_CONSENT_DENIED;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class OnboardingServiceWebTest {

    @Mock
    OnboardingRepository onboardingRepository;

    @Mock
    AuditUtilities auditUtilities;

    @Mock
    Utilities utilities;

    @Mock
    AdmissibilityRestConnector admissibilityRestConnector;

    @Mock
    SelfDeclarationRepository selfDeclarationRepository;

    @Mock
    ConsentMapper consentMapper;

    @Mock
    OnboardingProducer onboardingProducer;

    @Mock
    InitiativeRestConnector initiativeRestConnector;

    @Mock
    GeneralWebMapper generalWebMapper;

    @Mock
  private InitiativeWebMapper initiativeWebMapper;

  @Spy
    @InjectMocks
  private OnboardingServiceWebImpl onboardingServiceWeb;

  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private static final Locale ACCEPT_LANGUAGE = Locale.ITALIAN;

  private InitiativeDTO initiativeDTO;
  private InitiativeWebDTO initiativeWebDTO;
  private InitiativeGeneralWebDTO initiativeGeneralWebDTO;

  @BeforeEach
  void setUp() {
    InitiativeAdditionalDTO additional = new InitiativeAdditionalDTO();
    additional.setPrivacyLink("privacyLink");
    additional.setTcLink("TcLink");

    InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
    beneficiaryRule.setSelfDeclarationCriteria(List.of(
            new SelfCriteriaBooleanTypeDTO("boolean_type", "test description", "test sub description", true, SelfCriteriaBooleanTypeCode.ISEE)
    ));

    initiativeDTO = new InitiativeDTO();
    initiativeDTO.setAdditionalInfo(additional);
    initiativeDTO.setBeneficiaryRule(beneficiaryRule);

    GeneralWebMapper generalWebMapper = new GeneralWebMapper();

    InitiativeGeneralDTO initiativeGeneralDTO = new InitiativeGeneralDTO();
    initiativeGeneralDTO.setStartDate(LocalDate.MIN);
    initiativeGeneralDTO.setEndDate(LocalDate.MAX);
    Map<String, String> language = new HashMap<>();
    language.put(Locale.ITALIAN.getLanguage(), "it");
    initiativeGeneralDTO.setDescriptionMap(language);

    initiativeGeneralWebDTO = generalWebMapper.map(initiativeGeneralDTO, ACCEPT_LANGUAGE);

    initiativeWebDTO = new InitiativeWebDTO(additional, beneficiaryRule, initiativeGeneralWebDTO);

  }

  @Test
  void getInitiativeWeb_shouldReturnMappedDto_whenInitiativeExists() {
    when(onboardingServiceWeb.getInitiative(INITIATIVE_ID)).thenReturn(initiativeDTO);
    when(initiativeWebMapper.map(initiativeDTO, initiativeGeneralWebDTO)).thenReturn(initiativeWebDTO);

    InitiativeWebDTO result = onboardingServiceWeb.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE);

    assertNotNull(result);
    assertEquals(initiativeWebDTO, result);

    verify(onboardingServiceWeb, times(1)).getInitiative(INITIATIVE_ID);
    verify(initiativeWebMapper, times(1)).map(initiativeDTO, initiativeGeneralWebDTO);
  }

  @Test
  void getInitiativeWeb_shouldReturnNull_whenInitiativeDoesNotExist() {
    when(onboardingServiceWeb.getInitiative(INITIATIVE_ID)).thenReturn(null);

    InitiativeWebDTO result = onboardingServiceWeb.getInitiativeWeb(INITIATIVE_ID, ACCEPT_LANGUAGE);

    assertNull(result);

    verify(onboardingServiceWeb, times(1)).getInitiative(INITIATIVE_ID);
    verifyNoInteractions(initiativeWebMapper);
  }

    @Test
    void testSaveConsentWeb_SaveIsCalled_WhenOnboardingNotExists() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";
        String mail = "test@mail.com";

        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail(mail);
        consent.setUserMailConfirmation(mail);
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);

        doReturn(null).when(onboardingServiceWeb).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        InitiativeDTO initiativeTestDTO = new InitiativeDTO();
        initiativeTestDTO.setInitiativeId(initiativeId);
        initiativeTestDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        beneficiaryRule.setAutomatedCriteria(new ArrayList<>());
        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeTestDTO.setBeneficiaryRule(beneficiaryRule);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        general.setRankingStartDate(LocalDate.of(2025, 7, 31));
        general.setStartDate(LocalDate.of(2025, 7, 27));
        general.setEndDate(LocalDate.of(2025, 8, 11));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeTestDTO.setGeneral(general);

        InitiativeAdditionalDTO additionalInfo = new InitiativeAdditionalDTO();
        additionalInfo.setServiceId("dummyServiceId");
        initiativeTestDTO.setAdditionalInfo(additionalInfo);

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeTestDTO);

        when(consentMapper.map(any())).thenAnswer(invocation -> {
            Onboarding onboarding = invocation.getArgument(0);
            return OnboardingDTO.builder()
                    .userId(onboarding.getUserId())
                    .initiativeId(onboarding.getInitiativeId())
                    .status(onboarding.getStatus())
                    .build();
        });

        when(onboardingRepository.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        doNothing().when(onboardingServiceWeb).checkBudget(any(InitiativeDTO.class), any(Onboarding.class));

        onboardingServiceWeb.saveConsentWeb(consent, userId);

        verify(onboardingRepository, times(1)).save(any(Onboarding.class));
        verify(onboardingProducer, times(1)).sendSaveConsent(any(OnboardingDTO.class));
    }

    @Test
    void testSaveConsentWeb_throwsEmailNotMatchedException_whenEmailsDoNotMatch() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("email1@mail.com");
        consent.setUserMailConfirmation("email2@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);

        doReturn(null).when(onboardingServiceWeb).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        org.junit.jupiter.api.Assertions.assertThrows(EmailNotMatchedException.class, () -> onboardingServiceWeb.saveConsentWeb(consent, userId));

        verify(onboardingRepository, never()).save(any());
    }

    @Test
    void testSaveConsentWeb_throwsTosNotConfirmedException_whenTosNotConfirmed() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(false);
        consent.setPdndAccept(true);

        doReturn(null).when(onboardingServiceWeb).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        org.junit.jupiter.api.Assertions.assertThrows(TosNotConfirmedException.class, () -> onboardingServiceWeb.saveConsentWeb(consent, userId));

        verify(onboardingRepository, never()).save(any());
    }

    @Test
    void testSaveConsentWeb_throwsPDNDConsentDeniedException_whenAutomatedCriteriaExists_andPdndAcceptFalse() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(false);

        doReturn(null).when(onboardingServiceWeb).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        InitiativeDTO initiativeTestDTO = new InitiativeDTO();
        initiativeTestDTO.setInitiativeId(initiativeId);
        initiativeTestDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        beneficiaryRule.setAutomatedCriteria(new ArrayList<>());

        AutomatedCriteriaDTO criteria = new AutomatedCriteriaDTO();
        beneficiaryRule.getAutomatedCriteria().add(criteria);

        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeTestDTO.setBeneficiaryRule(beneficiaryRule);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        general.setRankingStartDate(LocalDate.of(2025, 7, 31));
        general.setStartDate(LocalDate.of(2025, 7, 27));
        general.setEndDate(LocalDate.of(2025, 8, 11));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeTestDTO.setGeneral(general);

        initiativeTestDTO.setAdditionalInfo(new InitiativeAdditionalDTO());
        initiativeTestDTO.getAdditionalInfo().setServiceId("serviceId");

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeTestDTO);

        doNothing().when(onboardingServiceWeb).checkDates(any(), any());
        doNothing().when(onboardingServiceWeb).checkBudget(any(), any());

        org.junit.jupiter.api.Assertions.assertThrows(PDNDConsentDeniedException.class, () -> onboardingServiceWeb.saveConsentWeb(consent, userId));

        verify(onboardingRepository, times(1)).save(argThat(onboarding ->
                OnboardingWorkflowConstants.ONBOARDING_KO.equals(onboarding.getStatus()) &&
                        PDND_CONSENT_DENIED.equals(onboarding.getDetailKO())
        ));

        verify(auditUtilities, times(1)).logOnboardingKOWithReason(eq(userId), eq(initiativeId), any(), any());
    }



    @Test
    void testSaveConsentWeb_ReturnsWhenStatusIsIdempotent() {
        String initiativeId = "INIT";
        String userId = "USER";

        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("mail@mail.com");
        consent.setUserMailConfirmation("mail@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);

        Onboarding onboarding = new Onboarding(initiativeId, userId);
        onboarding.setStatus(OnboardingWorkflowConstants.STATUS_IDEMPOTENT.iterator().next());

        doReturn(onboarding).when(onboardingServiceWeb).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        onboardingServiceWeb.saveConsentWeb(consent, userId);

        verify(onboardingRepository, never()).save(any());
        verify(onboardingProducer, never()).sendSaveConsent(any());
    }

    @Test
    void testSaveConsentWeb_CallsCheckStatusWhenStatusNotIdempotent() {
        String initiativeId = "INIT";
        String userId = "USER";

        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("mail@mail.com");
        consent.setUserMailConfirmation("mail@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);

        Onboarding onboarding = new Onboarding(initiativeId, userId);
        onboarding.setStatus("NON_IDEMPOTENT_STATUS");

        doReturn(onboarding).when(onboardingServiceWeb).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        doNothing().when(onboardingServiceWeb).checkStatus(onboarding);

        onboardingServiceWeb.saveConsentWeb(consent, userId);

        verify(onboardingServiceWeb).checkStatus(onboarding);
    }

    @Test
    void testSaveConsentWeb_AllowsConsent_WhenAutomatedCriteriaEmpty() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(false);

        doReturn(null).when(onboardingServiceWeb).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        InitiativeDTO initiativeTestDTO = new InitiativeDTO();
        initiativeTestDTO.setInitiativeId(initiativeId);
        initiativeTestDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        beneficiaryRule.setAutomatedCriteria(new ArrayList<>());
        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeTestDTO.setBeneficiaryRule(beneficiaryRule);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        general.setRankingStartDate(LocalDate.of(2025, 7, 31));
        general.setStartDate(LocalDate.of(2025, 7, 27));
        general.setEndDate(LocalDate.of(2025, 8, 11));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeTestDTO.setGeneral(general);

        InitiativeAdditionalDTO additional = new InitiativeAdditionalDTO();
        additional.setServiceId("serviceId");
        initiativeTestDTO.setAdditionalInfo(additional);

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeTestDTO);

        doNothing().when(onboardingServiceWeb).checkDates(any(), any());
        doNothing().when(onboardingServiceWeb).checkBudget(any(), any());

        when(consentMapper.map(any())).thenAnswer(invocation -> {
            Onboarding onboarding = invocation.getArgument(0);
            return OnboardingDTO.builder()
                    .userId(onboarding.getUserId())
                    .initiativeId(onboarding.getInitiativeId())
                    .status(onboarding.getStatus())
                    .build();
        });

        when(onboardingRepository.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        onboardingServiceWeb.saveConsentWeb(consent, userId);

        verify(onboardingRepository, times(1)).save(any(Onboarding.class));
        verify(onboardingProducer, times(1)).sendSaveConsent(any(OnboardingDTO.class));
    }

    @Test
    void testSaveConsentWeb_AllowsConsent_WhenAutomatedCriteriaPresentAndPdndAccepted() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);

        doReturn(null).when(onboardingServiceWeb).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        InitiativeDTO initiativeTestDTO = new InitiativeDTO();
        initiativeTestDTO.setInitiativeId(initiativeId);
        initiativeTestDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        List<AutomatedCriteriaDTO> autoCriteria = new ArrayList<>();
        autoCriteria.add(new AutomatedCriteriaDTO());
        beneficiaryRule.setAutomatedCriteria(autoCriteria);
        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeTestDTO.setBeneficiaryRule(beneficiaryRule);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        general.setRankingStartDate(LocalDate.of(2025, 7, 31));
        general.setStartDate(LocalDate.of(2025, 7, 27));
        general.setEndDate(LocalDate.of(2025, 8, 11));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeTestDTO.setGeneral(general);

        InitiativeAdditionalDTO additional = new InitiativeAdditionalDTO();
        additional.setServiceId("serviceId");
        initiativeTestDTO.setAdditionalInfo(additional);

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeTestDTO);

        doNothing().when(onboardingServiceWeb).checkDates(any(), any());
        doNothing().when(onboardingServiceWeb).checkBudget(any(), any());
        doNothing().when(onboardingServiceWeb).selfDeclaration(any(), any(), any());

        when(consentMapper.map(any(Onboarding.class))).thenAnswer(invocation -> {
            Onboarding onboarding = invocation.getArgument(0);
            return OnboardingDTO.builder()
                    .userId(onboarding.getUserId())
                    .initiativeId(onboarding.getInitiativeId())
                    .status(onboarding.getStatus())
                    .build();
        });

        when(onboardingRepository.save(any(Onboarding.class))).thenAnswer(invocation -> invocation.getArgument(0));

        onboardingServiceWeb.saveConsentWeb(consent, userId);

        verify(onboardingRepository, times(1)).save(any(Onboarding.class));
        verify(onboardingProducer, times(1)).sendSaveConsent(any(OnboardingDTO.class));
    }
}
