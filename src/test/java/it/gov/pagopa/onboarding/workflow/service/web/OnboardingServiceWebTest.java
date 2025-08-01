package it.gov.pagopa.onboarding.workflow.service.web;

import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.admissibility.InitiativeStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.*;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.ConsentPutWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.GeneralWebMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.InitiativeWebMapper;
import it.gov.pagopa.onboarding.workflow.event.producer.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.exception.custom.EmailNotMatchedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.PDNDConsentDeniedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.TosNotConfirmedException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import it.gov.pagopa.onboarding.workflow.repository.SelfDeclarationRepository;
import it.gov.pagopa.onboarding.workflow.service.common.OnboardingServiceCommonImpl;
import it.gov.pagopa.onboarding.workflow.utils.AuditUtilities;
import it.gov.pagopa.onboarding.workflow.utils.Utilities;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.PDND_CONSENT_DENIED;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

class OnboardingServiceWebImplTest {

    @Spy
    @InjectMocks
    OnboardingServiceWebImpl onboardingServiceWeb;

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
    InitiativeWebMapper initiativeWebMapper;

    @Mock
    GeneralWebMapper generalWebMapper;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        try {
            Field repoField = OnboardingServiceCommonImpl.class.getDeclaredField("onboardingRepository");
            repoField.setAccessible(true);
            repoField.set(onboardingServiceWeb, onboardingRepository);

            Field admissibilityField = OnboardingServiceCommonImpl.class.getDeclaredField("admissibilityRestConnector");
            admissibilityField.setAccessible(true);
            admissibilityField.set(onboardingServiceWeb, admissibilityRestConnector);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    void testSaveConsentWeb_SaveIsCalled_WhenOnboardingNotExists() {
        // Arrange
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";
        String mail = "test@mail.com";

        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail(mail);
        consent.setUserMailConfirmation(mail);
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);

        doReturn(null).when(onboardingServiceWeb).findByInitiativeIdAndUserId(initiativeId, userId);

        InitiativeDTO initiativeDTO = new InitiativeDTO();
        initiativeDTO.setInitiativeId(initiativeId);
        initiativeDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        beneficiaryRule.setAutomatedCriteria(new ArrayList<>());
        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeDTO.setBeneficiaryRule(beneficiaryRule);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        general.setRankingStartDate(LocalDate.of(2025, 7, 31));
        general.setStartDate(LocalDate.of(2025, 7, 27));
        general.setEndDate(LocalDate.of(2025, 8, 11));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));  // Necessario per checkBudget
        initiativeDTO.setGeneral(general);

        InitiativeAdditionalDTO additionalInfo = new InitiativeAdditionalDTO();
        additionalInfo.setServiceId("dummyServiceId");
        initiativeDTO.setAdditionalInfo(additionalInfo);

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeDTO);

        InitiativeStatusDTO initiativeStatusDTO = mock(InitiativeStatusDTO.class);
        when(initiativeStatusDTO.isBudgetAvailable()).thenReturn(true);
        when(initiativeStatusDTO.getStatus()).thenReturn("ACTIVE");
        when(admissibilityRestConnector.getInitiativeStatus(initiativeId)).thenReturn(initiativeStatusDTO);

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

        doReturn(null).when(onboardingServiceWeb).findByInitiativeIdAndUserId(initiativeId, userId);

        org.junit.jupiter.api.Assertions.assertThrows(EmailNotMatchedException.class, () -> {
            onboardingServiceWeb.saveConsentWeb(consent, userId);
        });

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

        doReturn(null).when(onboardingServiceWeb).findByInitiativeIdAndUserId(initiativeId, userId);

        org.junit.jupiter.api.Assertions.assertThrows(TosNotConfirmedException.class, () -> {
            onboardingServiceWeb.saveConsentWeb(consent, userId);
        });

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

        doReturn(null).when(onboardingServiceWeb).findByInitiativeIdAndUserId(initiativeId, userId);

        InitiativeDTO initiativeDTO = new InitiativeDTO();
        initiativeDTO.setInitiativeId(initiativeId);
        initiativeDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        beneficiaryRule.setAutomatedCriteria(new ArrayList<>());

        AutomatedCriteriaDTO criteria = new AutomatedCriteriaDTO();
        beneficiaryRule.getAutomatedCriteria().add(criteria);

        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeDTO.setBeneficiaryRule(beneficiaryRule);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        general.setRankingStartDate(LocalDate.of(2025, 7, 31));
        general.setStartDate(LocalDate.of(2025, 7, 27));
        general.setEndDate(LocalDate.of(2025, 8, 11));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeDTO.setGeneral(general);

        initiativeDTO.setAdditionalInfo(new InitiativeAdditionalDTO());
        initiativeDTO.getAdditionalInfo().setServiceId("serviceId");

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeDTO);

        InitiativeStatusDTO initiativeStatusDTO = mock(InitiativeStatusDTO.class);
        when(initiativeStatusDTO.isBudgetAvailable()).thenReturn(true);
        when(initiativeStatusDTO.getStatus()).thenReturn("ACTIVE");
        when(admissibilityRestConnector.getInitiativeStatus(initiativeId)).thenReturn(initiativeStatusDTO);

        doNothing().when(onboardingServiceWeb).checkDates(any(), any());
        doNothing().when(onboardingServiceWeb).checkBudget(any(), any());

        org.junit.jupiter.api.Assertions.assertThrows(PDNDConsentDeniedException.class, () -> {
            onboardingServiceWeb.saveConsentWeb(consent, userId);
        });

        verify(onboardingRepository, times(1)).save(argThat(onboarding ->
                OnboardingWorkflowConstants.ONBOARDING_KO.equals(onboarding.getStatus()) &&
                        PDND_CONSENT_DENIED.equals(onboarding.getDetailKO())
        ));

        verify(auditUtilities, times(1)).logOnboardingKOWithReason(eq(userId), eq(initiativeId), any(), any());
    }

    @Test
    void testGetInitiativeWeb_ShouldReturnMappedDTO() {
        String initiativeId = "TEST_INITIATIVE";
        Locale locale = Locale.ITALIAN;

        InitiativeDTO initiativeDTO = new InitiativeDTO();
        InitiativeGeneralDTO generalDTO = new InitiativeGeneralDTO();
        initiativeDTO.setGeneral(generalDTO);

        InitiativeGeneralWebDTO generalWebDTO = mock(InitiativeGeneralWebDTO.class);
        InitiativeWebDTO initiativeWebDTO = mock(InitiativeWebDTO.class);

        doReturn(initiativeDTO).when(onboardingServiceWeb).getInitiative(initiativeId);
        when(generalWebMapper.map(generalDTO, locale)).thenReturn(generalWebDTO);
        when(initiativeWebMapper.map(initiativeDTO, generalWebDTO)).thenReturn(initiativeWebDTO);

        InitiativeWebDTO result = onboardingServiceWeb.getInitiativeWeb(initiativeId, locale);

        assertNotNull(result);
        assertEquals(initiativeWebDTO, result);

        verify(onboardingServiceWeb).getInitiative(initiativeId);
        verify(generalWebMapper).map(generalDTO, locale);
        verify(initiativeWebMapper).map(initiativeDTO, generalWebDTO);
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

        doReturn(onboarding).when(onboardingServiceWeb).findByInitiativeIdAndUserId(initiativeId, userId);

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

        doReturn(onboarding).when(onboardingServiceWeb).findByInitiativeIdAndUserId(initiativeId, userId);

        doNothing().when(onboardingServiceWeb).checkStatus(onboarding);

        onboardingServiceWeb.saveConsentWeb(consent, userId);

        verify(onboardingServiceWeb).checkStatus(onboarding);
    }

    @Test
    void testSaveConsentWeb_AllowsConsent_WhenAutomatedCriteriaEmpty() {
        // Arrange
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutWebDTO consent = new ConsentPutWebDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(false);

        doReturn(null).when(onboardingServiceWeb).findByInitiativeIdAndUserId(initiativeId, userId);

        InitiativeDTO initiativeDTO = new InitiativeDTO();
        initiativeDTO.setInitiativeId(initiativeId);
        initiativeDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        beneficiaryRule.setAutomatedCriteria(new ArrayList<>());
        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeDTO.setBeneficiaryRule(beneficiaryRule);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        general.setRankingStartDate(LocalDate.of(2025, 7, 31));
        general.setStartDate(LocalDate.of(2025, 7, 27));
        general.setEndDate(LocalDate.of(2025, 8, 11));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeDTO.setGeneral(general);

        InitiativeAdditionalDTO additional = new InitiativeAdditionalDTO();
        additional.setServiceId("serviceId");
        initiativeDTO.setAdditionalInfo(additional);

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeDTO);

        InitiativeStatusDTO initiativeStatusDTO = mock(InitiativeStatusDTO.class);
        when(initiativeStatusDTO.isBudgetAvailable()).thenReturn(true);
        when(initiativeStatusDTO.getStatus()).thenReturn("ACTIVE");
        when(admissibilityRestConnector.getInitiativeStatus(initiativeId)).thenReturn(initiativeStatusDTO);

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
        consent.setPdndAccept(true); // <-- PDND accettato

        doReturn(null).when(onboardingServiceWeb).findByInitiativeIdAndUserId(initiativeId, userId);

        InitiativeDTO initiativeDTO = new InitiativeDTO();
        initiativeDTO.setInitiativeId(initiativeId);
        initiativeDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        List<AutomatedCriteriaDTO> autoCriteria = new ArrayList<>();
        autoCriteria.add(new AutomatedCriteriaDTO()); // <-- non vuoto
        beneficiaryRule.setAutomatedCriteria(autoCriteria);
        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeDTO.setBeneficiaryRule(beneficiaryRule);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        general.setRankingStartDate(LocalDate.of(2025, 7, 31));
        general.setStartDate(LocalDate.of(2025, 7, 27));
        general.setEndDate(LocalDate.of(2025, 8, 11));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeDTO.setGeneral(general);

        InitiativeAdditionalDTO additional = new InitiativeAdditionalDTO();
        additional.setServiceId("serviceId");
        initiativeDTO.setAdditionalInfo(additional);

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeDTO);

        InitiativeStatusDTO initiativeStatusDTO = mock(InitiativeStatusDTO.class);
        when(initiativeStatusDTO.isBudgetAvailable()).thenReturn(true);
        when(initiativeStatusDTO.getStatus()).thenReturn("ACTIVE");
        when(admissibilityRestConnector.getInitiativeStatus(initiativeId)).thenReturn(initiativeStatusDTO);

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
