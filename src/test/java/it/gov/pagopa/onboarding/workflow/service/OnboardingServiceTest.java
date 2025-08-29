package it.gov.pagopa.onboarding.workflow.service;

import com.mongodb.MongoException;
import com.mongodb.MongoQueryException;
import com.mongodb.ServerAddress;
import feign.FeignException;
import feign.Request;
import feign.RequestTemplate;
import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.decrypt.DecryptRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.*;
import it.gov.pagopa.onboarding.workflow.dto.admissibility.InitiativeStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.*;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.GeneralWebMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.InitiativeWebMapper;
import it.gov.pagopa.onboarding.workflow.enums.ChannelType;
import it.gov.pagopa.onboarding.workflow.enums.SelfCriteriaMultiTypeCode;
import it.gov.pagopa.onboarding.workflow.event.producer.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.event.producer.OutcomeProducer;
import it.gov.pagopa.onboarding.workflow.exception.custom.*;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.model.SelfDeclaration;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import it.gov.pagopa.onboarding.workflow.repository.SelfDeclarationRepository;
import it.gov.pagopa.onboarding.workflow.utils.AuditUtilities;
import it.gov.pagopa.onboarding.workflow.utils.Utilities;
import org.bson.BsonDocument;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.UncategorizedMongoDbException;
import org.springframework.data.mongodb.core.query.Criteria;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Stream;

import static com.mongodb.assertions.Assertions.assertTrue;
import static com.mongodb.assertions.Assertions.fail;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.*;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.GENERIC_ERROR;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class OnboardingServiceTest {

    @Mock
    OnboardingRepository onboardingRepositoryMock;

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

    @Mock
    private InitiativeWebDTO initiativeWebDTO;

    @Mock
    private InitiativeGeneralWebDTO initiativeGeneralWebDTO;

    @Mock
    private InitiativeDTO initiativeDTO;

    @Autowired
    private OnboardingServiceImpl onboardingService;

    @Mock
    private OutcomeProducer outcomeProducer;
    @Mock
    private DecryptRestConnector decryptRestConnector;


    private static final int PAGE_SIZE = 10;
    private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
    private static final String USER_ID = "TEST_USER_ID";
    private static final String ONBOARDING_BUDGET_EXHAUSTED = "ONBOARDING_BUDGET_EXHAUSTED";
    private static final String ONBOARDING_USER_UNSUBSCRIBED = "ONBOARDING_USER_UNSUBSCRIBED";
    private static final String ONBOARDING_INITIATIVE_STATUS_NOT_PUBLISHED = "ONBOARDING_INITIATIVE_STATUS_NOT_PUBLISHED";
    private static final String ONBOARDING_INITIATIVE_ENDED = "ONBOARDING_INITIATIVE_ENDED";
    private static final Locale ACCEPT_LANGUAGE = Locale.ITALIAN;

    private static final String FAMILY_ID = "TEST_FAMILY_ID";
    private static final LocalDate OPERATION_DATE = LocalDate.now();
    private static final LocalDateTime START_DATE = LocalDateTime.now();
    private static final LocalDateTime END_DATE = LocalDateTime.now();
    private static final String STATUS = "STATUS";
    private static final String SERVICE_ID = "SERVICE_ID";
    private static final String INITIATIVE_NAME = "INITIATIVE_NAME";
    private static final String ORGANIZATION_NAME = "TEST_ORGANIZATION_NAME";
    private static final String CHANNEL = "CHANNEL";
    private static final String PII = "PII_TEST";
    public static final String OPERATION_TYPE_DELETE_INITIATIVE = "DELETE_INITIATIVE";

    private static final BigDecimal BUDGET = BigDecimal.TEN;
    private static final BigDecimal BENEFICIARY_BUDGET = BigDecimal.ONE;
    private static final String INVALID_INITIATIVE = "INVALID_INITIATIVE_ID";
    private static final String OUT_OF_RANKING = "OUT_OF_RANKING";
    private static final String INITIATIVE_REWARD_TYPE_DISCOUNT = "DISCOUNT";
    private static final String BENEFICIARY_TYPE_NF = "NF";
    private static final EvaluationDTO EVALUATION_DTO =
            new EvaluationDTO(
                    USER_ID, null, INITIATIVE_ID, INITIATIVE_ID, OPERATION_DATE, INITIATIVE_ID, OnboardingWorkflowConstants.ONBOARDING_OK,
                    OPERATION_DATE.atStartOfDay(), OPERATION_DATE.atStartOfDay(), List.of(),
                    500L, INITIATIVE_REWARD_TYPE_DISCOUNT, ORGANIZATION_NAME, false, SERVICE_ID);
    private static final EvaluationDTO EVALUATION_DTO_ONBOARDING_KO_OUT_OF_RANKING =
            new EvaluationDTO(
                    USER_ID, null, INITIATIVE_ID, INITIATIVE_ID, OPERATION_DATE, INITIATIVE_ID, OnboardingWorkflowConstants.ONBOARDING_KO,
                    OPERATION_DATE.atStartOfDay(), OPERATION_DATE.atStartOfDay(),
                    List.of(new OnboardingRejectionReason(INVALID_INITIATIVE, INVALID_INITIATIVE, null, null, null),
                            new OnboardingRejectionReason(OUT_OF_RANKING, "CITIZEN_OUT_OF_RANKING", null, null, null)),
                    500L, INITIATIVE_REWARD_TYPE_DISCOUNT, ORGANIZATION_NAME, false, SERVICE_ID);

    private static final EvaluationDTO EVALUATION_DTO_ONBOARDING_KO =
            new EvaluationDTO(
                    USER_ID, null, INITIATIVE_ID, INITIATIVE_ID, OPERATION_DATE, INITIATIVE_ID, OnboardingWorkflowConstants.ONBOARDING_KO,
                    OPERATION_DATE.atStartOfDay(), OPERATION_DATE.atStartOfDay(),
                    List.of(new OnboardingRejectionReason(INVALID_INITIATIVE, INVALID_INITIATIVE, null, null, null)),
                    500L, INITIATIVE_REWARD_TYPE_DISCOUNT, ORGANIZATION_NAME, false, SERVICE_ID);

    private static final InitiativeDTO INITIATIVE_DTO = new InitiativeDTO();
    private static final InitiativeDTO INITIATIVE_DTO_RANKING = new InitiativeDTO();
    private static final InitiativeDTO INITIATIVE_DTO_NO_PDND = new InitiativeDTO();
    private static final InitiativeDTO INITIATIVE_DTO_NO_SELF = new InitiativeDTO();
    private static final InitiativeDTO INITIATIVE_DTO_NO_CRITERIA = new InitiativeDTO();
    private static final InitiativeDTO INITIATIVE_DTO_WHITELIST = new InitiativeDTO();
    private static final InitiativeDTO INITIATIVE_DTO_KO_START_DATE = new InitiativeDTO();
    private static final InitiativeDTO INITIATIVE_DTO_KO_RANKING_START_DATE = new InitiativeDTO();
    private static final InitiativeDTO INITIATIVE_DTO_KO_END_DATE = new InitiativeDTO();
    private static final InitiativeDTO INITIATIVE_DTO_KO_RANKING_END_DATE = new InitiativeDTO();
    private static final InitiativeDTO INITIATIVE_DTO_KO = new InitiativeDTO();
    private static final InitiativeBeneficiaryRuleDTO INITIATIVE_BENEFICIARY_RULE_DTO = new InitiativeBeneficiaryRuleDTO();
    private static final InitiativeBeneficiaryRuleDTO INITIATIVE_BENEFICIARY_RULE_DTO_NO_PDND = new InitiativeBeneficiaryRuleDTO();
    private static final InitiativeBeneficiaryRuleDTO INITIATIVE_BENEFICIARY_RULE_DTO_NO_SELF = new InitiativeBeneficiaryRuleDTO();
    private static final InitiativeBeneficiaryRuleDTO INITIATIVE_BENEFICIARY_RULE_DTO_NO_CRITERIA = new InitiativeBeneficiaryRuleDTO();
    private static final InitiativeGeneralDTO GENERAL = new InitiativeGeneralDTO();
    private static final InitiativeGeneralDTO GENERAL_RANKING = new InitiativeGeneralDTO();
    private static final InitiativeGeneralDTO GENERAL_WHITELIST = new InitiativeGeneralDTO();
    private static final InitiativeGeneralDTO GENERAL_KO_START_DATE = new InitiativeGeneralDTO();
    private static final InitiativeGeneralDTO GENERAL_KO_RANKING_START_DATE = new InitiativeGeneralDTO();
    private static final InitiativeGeneralDTO GENERAL_KO_END_DATE = new InitiativeGeneralDTO();
    private static final InitiativeGeneralDTO GENERAL_KO_RANKING_END_DATE = new InitiativeGeneralDTO();
    private static final InitiativeAdditionalDTO ADDITIONAL_DTO_WHITELIST = new InitiativeAdditionalDTO();
    private static final CitizenStatusDTO CITIZEN_STATUS_DTO = new CitizenStatusDTO();
    private static final CitizenStatusDTO CITIZEN_STATUS_DTO_KO = new CitizenStatusDTO();

    private static final InitiativeStatusDTO INITIATIVE_STATUS_DTO = InitiativeStatusDTO.builder()
            .status(OnboardingWorkflowConstants.PUBLISHED)
            .budgetAvailable(true)
            .build();

    private static final InitiativeStatusDTO INITIATIVE_NOT_BUDGET_DTO = InitiativeStatusDTO.builder()
            .status(OnboardingWorkflowConstants.PUBLISHED)
            .budgetAvailable(false)
            .build();
    private static final OnboardingNotificationDTO ONBOARDING_NOTIFICATION_DTO = OnboardingNotificationDTO.builder()
            .initiativeId(INITIATIVE_ID)
            .serviceId(SERVICE_ID)
            .operationType(OnboardingWorkflowConstants.ALLOWED_CITIZEN_PUBLISH)
            .userId(USER_ID)
            .initiativeName(INITIATIVE_NAME)
            .build();
    private static final AutomatedCriteriaDTO AUTOMATED_CRITERIA_DTO = new AutomatedCriteriaDTO();
    private static final OnboardingNotificationDTO ONBOARDING_NOTIFICATION_DTO_IBAN = OnboardingNotificationDTO.builder()
            .initiativeId(INITIATIVE_ID)
            .serviceId(SERVICE_ID)
            .operationType("CHECKIBAN")
            .userId(USER_ID)
            .initiativeName(INITIATIVE_NAME)
            .build();

    static {
        AUTOMATED_CRITERIA_DTO.setCode("BIRTHDATE");

        CITIZEN_STATUS_DTO.setStatus(true);

        CITIZEN_STATUS_DTO_KO.setStatus(false);

        GENERAL.setBeneficiaryKnown(false);
        GENERAL.setStartDate(LocalDate.MIN);
        GENERAL.setEndDate(LocalDate.MAX);
        GENERAL.setBudget(BUDGET);
        GENERAL.setBeneficiaryBudget(BENEFICIARY_BUDGET);
        GENERAL.setRankingEnabled(Boolean.FALSE);

        GENERAL_RANKING.setBeneficiaryKnown(false);
        GENERAL_RANKING.setStartDate(LocalDate.MIN);
        GENERAL_RANKING.setEndDate(LocalDate.MAX);
        GENERAL_RANKING.setRankingStartDate(LocalDate.MIN);
        GENERAL_RANKING.setRankingEndDate(LocalDate.MAX);
        GENERAL_RANKING.setBudget(BUDGET);
        GENERAL_RANKING.setBeneficiaryBudget(BENEFICIARY_BUDGET);
        GENERAL_RANKING.setRankingEnabled(Boolean.TRUE);

        GENERAL_WHITELIST.setBeneficiaryKnown(true);
        GENERAL_WHITELIST.setStartDate(LocalDate.MIN);
        GENERAL_WHITELIST.setEndDate(LocalDate.MAX);
        GENERAL_WHITELIST.setBudget(BUDGET);
        GENERAL_WHITELIST.setBeneficiaryBudget(BENEFICIARY_BUDGET);

        GENERAL_KO_START_DATE.setBeneficiaryKnown(false);
        GENERAL_KO_START_DATE.setStartDate(LocalDate.MAX);
        GENERAL_KO_START_DATE.setEndDate(LocalDate.MAX);

        GENERAL_KO_RANKING_START_DATE.setBeneficiaryKnown(false);
        GENERAL_KO_RANKING_START_DATE.setRankingStartDate(LocalDate.MAX);
        GENERAL_KO_RANKING_START_DATE.setRankingEndDate(LocalDate.MAX);

        ADDITIONAL_DTO_WHITELIST.setServiceId(INITIATIVE_ID);

        GENERAL_KO_END_DATE.setBeneficiaryKnown(false);
        GENERAL_KO_END_DATE.setStartDate(LocalDate.MIN);
        GENERAL_KO_END_DATE.setEndDate(LocalDate.MIN);

        GENERAL_KO_RANKING_END_DATE.setBeneficiaryKnown(false);
        GENERAL_KO_RANKING_END_DATE.setRankingStartDate(LocalDate.MIN);
        GENERAL_KO_RANKING_END_DATE.setRankingEndDate(LocalDate.MIN);

        INITIATIVE_BENEFICIARY_RULE_DTO.setSelfDeclarationCriteria(
                List.of(new SelfCriteriaBoolDTO("boolean", "", true, "1"),
                        new SelfCriteriaMultiDTO("multi", "", List.of("Value", "Value2", "1"), "2"),
                        new SelfCriteriaTextDTO("text", "", "Value3", "3")));
        INITIATIVE_BENEFICIARY_RULE_DTO.setAutomatedCriteria(List.of(AUTOMATED_CRITERIA_DTO));

        INITIATIVE_BENEFICIARY_RULE_DTO_NO_PDND.setSelfDeclarationCriteria(
                List.of(new SelfCriteriaBoolDTO("boolean", "", true, "1"),
                        new SelfCriteriaMultiDTO("multi", "", List.of("Value", "Value2", "1"), "2"),
                        new SelfCriteriaTextDTO("text", "", "Value3", "3")));
        INITIATIVE_BENEFICIARY_RULE_DTO_NO_PDND.setAutomatedCriteria(List.of());

        INITIATIVE_BENEFICIARY_RULE_DTO_NO_SELF.setSelfDeclarationCriteria(List.of());
        INITIATIVE_BENEFICIARY_RULE_DTO_NO_SELF.setAutomatedCriteria(
                List.of(AUTOMATED_CRITERIA_DTO));

        INITIATIVE_BENEFICIARY_RULE_DTO_NO_CRITERIA.setSelfDeclarationCriteria(List.of());
        INITIATIVE_BENEFICIARY_RULE_DTO_NO_CRITERIA.setAutomatedCriteria(List.of());

        INITIATIVE_DTO.setInitiativeId(INITIATIVE_ID);
        INITIATIVE_DTO.setStatus("PUBLISHED");
        INITIATIVE_DTO.setGeneral(GENERAL);
        INITIATIVE_DTO.setBeneficiaryRule(INITIATIVE_BENEFICIARY_RULE_DTO);
        INITIATIVE_DTO.setInitiativeRewardType(INITIATIVE_REWARD_TYPE_DISCOUNT);

        INITIATIVE_DTO_RANKING.setInitiativeId(INITIATIVE_ID);
        INITIATIVE_DTO_RANKING.setStatus("PUBLISHED");
        INITIATIVE_DTO_RANKING.setGeneral(GENERAL_RANKING);
        INITIATIVE_DTO_RANKING.setBeneficiaryRule(INITIATIVE_BENEFICIARY_RULE_DTO);
        INITIATIVE_DTO_RANKING.setInitiativeRewardType(INITIATIVE_REWARD_TYPE_DISCOUNT);

        INITIATIVE_DTO_NO_PDND.setInitiativeId(INITIATIVE_ID);
        INITIATIVE_DTO_NO_PDND.setStatus("PUBLISHED");
        INITIATIVE_DTO_NO_PDND.setGeneral(GENERAL);
        INITIATIVE_DTO_NO_PDND.setBeneficiaryRule(INITIATIVE_BENEFICIARY_RULE_DTO_NO_PDND);

        INITIATIVE_DTO_NO_SELF.setInitiativeId(INITIATIVE_ID);
        INITIATIVE_DTO_NO_SELF.setStatus("PUBLISHED");
        INITIATIVE_DTO_NO_SELF.setGeneral(GENERAL);
        INITIATIVE_DTO_NO_SELF.setBeneficiaryRule(INITIATIVE_BENEFICIARY_RULE_DTO_NO_SELF);

        INITIATIVE_DTO_NO_CRITERIA.setInitiativeId(INITIATIVE_ID);
        INITIATIVE_DTO_NO_CRITERIA.setStatus("PUBLISHED");
        INITIATIVE_DTO_NO_CRITERIA.setGeneral(GENERAL);
        INITIATIVE_DTO_NO_CRITERIA.setBeneficiaryRule(INITIATIVE_BENEFICIARY_RULE_DTO_NO_CRITERIA);

        INITIATIVE_DTO_WHITELIST.setInitiativeId(INITIATIVE_ID);
        INITIATIVE_DTO_WHITELIST.setStatus("PUBLISHED");
        INITIATIVE_DTO_WHITELIST.setGeneral(GENERAL_WHITELIST);
        INITIATIVE_DTO_WHITELIST.setAdditionalInfo(ADDITIONAL_DTO_WHITELIST);

        INITIATIVE_DTO_KO_START_DATE.setStatus("PUBLISHED");
        INITIATIVE_DTO_KO_START_DATE.setGeneral(GENERAL_KO_START_DATE);

        INITIATIVE_DTO_KO_RANKING_START_DATE.setStatus("PUBLISHED");
        INITIATIVE_DTO_KO_RANKING_START_DATE.setGeneral(GENERAL_KO_RANKING_START_DATE);

        INITIATIVE_DTO_KO_END_DATE.setStatus("PUBLISHED");
        INITIATIVE_DTO_KO_END_DATE.setGeneral(GENERAL_KO_END_DATE);

        INITIATIVE_DTO_KO_RANKING_END_DATE.setStatus("PUBLISHED");
        INITIATIVE_DTO_KO_RANKING_END_DATE.setGeneral(GENERAL_KO_RANKING_END_DATE);

        INITIATIVE_DTO_KO.setStatus("CLOSED");
    }

    @BeforeEach
    void setUp() {
        long delayTime = 100L;

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        LocalDate today = LocalDate.now();
        general.setStartDate(today.minusDays(1));
        general.setEndDate(today.plusDays(1));
        general.setRankingStartDate(today);
        general.setRankingEndDate(today.plusDays(2));
        general.setBeneficiaryKnown(false);
        general.setBudget(BigDecimal.valueOf(1000));
        general.setBeneficiaryBudget(BigDecimal.valueOf(100));

        initiativeDTO = new InitiativeDTO();
        initiativeDTO.setGeneral(general);
        initiativeDTO.setStatus("PUBLISHED");
        initiativeDTO.setInitiativeId("TEST_INITIATIVE_ID");

        InitiativeAdditionalDTO additional = new InitiativeAdditionalDTO();
        additional.setPrivacyLink("privacyLink");
        additional.setTcLink("TcLink");

        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        beneficiaryRule.setSelfDeclarationCriteria(List.of(
                new SelfCriteriaBooleanTypeDTO(
                        "boolean_type",
                        "test description",
                        "test sub description",
                        true,
                        SelfCriteriaBooleanTypeCode.ISEE
                )
        ));

        initiativeDTO.setAdditionalInfo(additional);
        initiativeDTO.setBeneficiaryRule(beneficiaryRule);

        onboardingService = Mockito.spy(new OnboardingServiceImpl(
                PAGE_SIZE,
                delayTime,
                selfDeclarationRepository,
                consentMapper,
                onboardingProducer,
                outcomeProducer,
                initiativeRestConnector,
                decryptRestConnector,
                admissibilityRestConnector,
                auditUtilities,
                utilities,
                onboardingRepositoryMock,
                initiativeWebMapper,
                generalWebMapper
        ));


        generalWebMapper = new GeneralWebMapper();

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
    void initiativeDetailExists() {
        when(onboardingService.getInitiative(INITIATIVE_ID)).thenReturn(initiativeDTO);
        when(initiativeWebMapper.map(eq(initiativeDTO), any())).thenReturn(initiativeWebDTO);

        InitiativeWebDTO result = onboardingService.initiativeDetail(INITIATIVE_ID, ACCEPT_LANGUAGE);

        assertNotNull(result);
        assertEquals(initiativeWebDTO, result);

        verify(onboardingService, times(1)).getInitiative(INITIATIVE_ID);
        verify(initiativeWebMapper, times(1)).map(eq(initiativeDTO), any());
    }


    @Test
    void initiativeDetailDoesNotExist() {
        when(onboardingService.getInitiative(INITIATIVE_ID)).thenReturn(null);

        InitiativeWebDTO result = onboardingService.initiativeDetail(INITIATIVE_ID, ACCEPT_LANGUAGE);

        assertNull(result);

        verify(onboardingService, times(1)).getInitiative(INITIATIVE_ID);
        verifyNoInteractions(initiativeWebMapper);
    }

    @Test
    void testSaveConsent_SaveIsCalled_WhenOnboardingNotExists() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";
        String mail = "test@mail.com";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail(mail);
        consent.setUserMailConfirmation(mail);
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);
        consent.setChannel(ChannelType.WEB);

        doReturn(null).when(onboardingService)
                .findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        InitiativeDTO initiativeTestDTO = new InitiativeDTO();
        initiativeTestDTO.setInitiativeId(initiativeId);
        initiativeTestDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        beneficiaryRule.setAutomatedCriteria(new ArrayList<>());
        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeTestDTO.setBeneficiaryRule(beneficiaryRule);

        initiativeTestDTO.setGeneral(initiativeDTO.getGeneral());

        InitiativeAdditionalDTO additionalInfo = new InitiativeAdditionalDTO();
        additionalInfo.setServiceId("ServiceId");
        initiativeTestDTO.setAdditionalInfo(additionalInfo);

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId))
                .thenReturn(initiativeTestDTO);

        when(consentMapper.map(any())).thenAnswer(invocation -> {
            Onboarding onboarding = invocation.getArgument(0);
            return OnboardingDTO.builder()
                    .userId(onboarding.getUserId())
                    .initiativeId(onboarding.getInitiativeId())
                    .status(onboarding.getStatus())
                    .build();
        });

        when(onboardingRepositoryMock.save(any()))
                .thenAnswer(invocation -> invocation.getArgument(0));

        doNothing().when(onboardingService)
                .checkBudget(any(InitiativeDTO.class), any(Onboarding.class));

        onboardingService.saveOnboarding(consent, userId);

        verify(onboardingRepositoryMock, times(1)).save(any(Onboarding.class));
        verify(onboardingProducer, times(1)).sendSaveConsent(any(OnboardingDTO.class));
    }


    @Test
    void testSaveConsentAppIO_SaveIsCalled_WhenOnboardingNotExists() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);
        consent.setChannel(ChannelType.APP_IO);

        doReturn(null).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        InitiativeDTO initiativeTestDTO = new InitiativeDTO();
        initiativeTestDTO.setInitiativeId(initiativeId);
        initiativeTestDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        beneficiaryRule.setAutomatedCriteria(new ArrayList<>());
        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeTestDTO.setBeneficiaryRule(beneficiaryRule);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        LocalDate today = LocalDate.now();
        general.setStartDate(today);
        general.setEndDate(today.plusDays(1));
        general.setRankingStartDate(today);
        general.setRankingEndDate(today.plusDays(2));
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

        when(onboardingRepositoryMock.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        doNothing().when(onboardingService).checkBudget(any(InitiativeDTO.class), any(Onboarding.class));

        onboardingService.saveOnboarding(consent, userId);

        verify(onboardingRepositoryMock, times(1)).save(any(Onboarding.class));
        verify(onboardingProducer, times(1)).sendSaveConsent(any(OnboardingDTO.class));
    }


    @Test
    void testSaveOnboardingWeb_throwsEmailNotMatchedException_whenEmailsDoNotMatch() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test1@mail.com");
        consent.setUserMailConfirmation("test2@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);
        consent.setChannel(ChannelType.WEB);

        doReturn(null).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        org.junit.jupiter.api.Assertions.assertThrows(EmailNotMatchedException.class, () -> onboardingService.saveOnboarding(consent, userId));

        verify(onboardingRepositoryMock, never()).save(any());
    }

    @Test
    void testSaveOnboardingWeb_throwsTosNotConfirmedException_whenTosNotConfirmed() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(false);
        consent.setPdndAccept(true);
        consent.setChannel(ChannelType.WEB);

        doReturn(null).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        org.junit.jupiter.api.Assertions.assertThrows(TosNotConfirmedException.class, () -> onboardingService.saveOnboarding(consent, userId));

        verify(onboardingRepositoryMock, never()).save(any());
    }

    @Test
    void testSaveOnboardingAppIO_throwsTosNotConfirmedException_whenTosNotConfirmed() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setConfirmedTos(false);
        consent.setPdndAccept(true);
        consent.setChannel(ChannelType.APP_IO);

        doReturn(null).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        assertThrows(TosNotConfirmedException.class, () -> onboardingService.saveOnboarding(consent, userId));

        verify(onboardingRepositoryMock, never()).save(any());
        verify(onboardingProducer, never()).sendSaveConsent(any());
    }


    @Test
    void testSaveOnboardingWeb_throwsPDNDOnboardingDeniedException_whenAutomatedCriteriaExists_andPdndAcceptFalse() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(false);
        consent.setChannel(ChannelType.WEB);

        doReturn(null).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

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
        LocalDate today = LocalDate.now();
        general.setStartDate(today);
        general.setEndDate(today.plusDays(1));
        general.setRankingStartDate(today);
        general.setRankingEndDate(today.plusDays(2));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeTestDTO.setGeneral(general);


        initiativeTestDTO.setAdditionalInfo(new InitiativeAdditionalDTO());
        initiativeTestDTO.getAdditionalInfo().setServiceId("serviceId");

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeTestDTO);

        doNothing().when(onboardingService).checkDates(any(), any());
        doNothing().when(onboardingService).checkBudget(any(), any());

        org.junit.jupiter.api.Assertions.assertThrows(PDNDConsentDeniedException.class, () -> onboardingService.saveOnboarding(consent, userId));

        verify(onboardingRepositoryMock, times(1)).save(argThat(onboarding ->
                OnboardingWorkflowConstants.ONBOARDING_KO.equals(onboarding.getStatus()) &&
                        PDND_CONSENT_DENIED.equals(onboarding.getDetailKO())
        ));

        verify(auditUtilities, times(1)).logOnboardingKOWithReason(eq(userId), eq(initiativeId), any(), any());
    }

    @Test
    void testSaveOnboardingAppIO_throwsPDNDOnboardingDeniedException_whenAutomatedCriteriaExists_andPdndAcceptFalse() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setConfirmedTos(true);
        consent.setPdndAccept(false);
        consent.setChannel(ChannelType.APP_IO);

        doReturn(null).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        initiativeDTO = new InitiativeDTO();
        initiativeDTO.setInitiativeId(initiativeId);
        initiativeDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO ruleDTO = new InitiativeBeneficiaryRuleDTO();
        ruleDTO.setAutomatedCriteria(List.of(new AutomatedCriteriaDTO()));
        ruleDTO.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeDTO.setBeneficiaryRule(ruleDTO);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        LocalDate today = LocalDate.now();
        general.setStartDate(today);
        general.setEndDate(today.plusDays(1));
        general.setRankingStartDate(today);
        general.setRankingEndDate(today.plusDays(2));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeDTO.setGeneral(general);


        InitiativeAdditionalDTO additional = new InitiativeAdditionalDTO();
        additional.setServiceId("serviceId");
        initiativeDTO.setAdditionalInfo(additional);

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeDTO);

        doNothing().when(onboardingService).checkDates(any(), any());
        doNothing().when(onboardingService).checkBudget(any(), any());

        assertThrows(PDNDConsentDeniedException.class, () -> onboardingService.saveOnboarding(consent, userId));

        verify(onboardingRepositoryMock).save(argThat(onboarding ->
                OnboardingWorkflowConstants.ONBOARDING_KO.equals(onboarding.getStatus()) &&
                        PDND_CONSENT_DENIED.equals(onboarding.getDetailKO())
        ));

        verify(auditUtilities).logOnboardingKOWithReason(eq(userId), eq(initiativeId), any(), any());
    }


    @Test
    void testSaveOnboardingWeb_ReturnsWhenStatusIsIdempotent() {
        String initiativeId = "INIT";
        String userId = "USER";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);
        consent.setChannel(ChannelType.WEB);

        Onboarding onboarding = new Onboarding(initiativeId, userId);
        onboarding.setStatus(OnboardingWorkflowConstants.STATUS_IDEMPOTENT.iterator().next());

        doReturn(onboarding).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        onboardingService.saveOnboarding(consent, userId);

        verify(onboardingRepositoryMock, never()).save(any());
        verify(onboardingProducer, never()).sendSaveConsent(any());
    }

    @Test
    void testSaveOnboardingAppIO_ReturnsWhenStatusIsIdempotent() {
        String initiativeId = "INIT";
        String userId = "USER";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);
        consent.setChannel(ChannelType.APP_IO);

        Onboarding onboarding = new Onboarding(initiativeId, userId);
        onboarding.setStatus(OnboardingWorkflowConstants.STATUS_IDEMPOTENT.getFirst());

        doReturn(onboarding).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        onboardingService.saveOnboarding(consent, userId);

        verify(onboardingRepositoryMock, never()).save(any());
        verify(onboardingProducer, never()).sendSaveConsent(any());
    }

    @Test
    void testSaveOnboardingWeb_CallsCheckStatusWhenStatusNotIdempotent() {
        String initiativeId = "INIT";
        String userId = "USER";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);
        consent.setChannel(ChannelType.WEB);

        Onboarding onboarding = new Onboarding(initiativeId, userId);
        onboarding.setStatus("NON_IDEMPOTENT_STATUS");

        doReturn(onboarding).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        doNothing().when(onboardingService).checkStatus(onboarding);

        onboardingService.saveOnboarding(consent, userId);

        verify(onboardingService).checkStatus(onboarding);
    }

    @Test
    void testSaveOnboardingAppIO_CallsCheckStatusWhenStatusNotIdempotent() {
        String initiativeId = "INIT";
        String userId = "USER";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);
        consent.setChannel(ChannelType.APP_IO);

        Onboarding onboarding = new Onboarding(initiativeId, userId);
        onboarding.setStatus("NON_IDEMPOTENT");

        doReturn(onboarding).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);
        doNothing().when(onboardingService).checkStatus(onboarding);

        onboardingService.saveOnboarding(consent, userId);

        verify(onboardingService).checkStatus(onboarding);
    }

    @Test
    void testSaveOnboardingWeb_AllowsOnboarding_WhenAutomatedCriteriaEmpty() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(false);
        consent.setChannel(ChannelType.WEB);

        doReturn(null).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        InitiativeDTO initiativeTestDTO = new InitiativeDTO();
        initiativeTestDTO.setInitiativeId(initiativeId);
        initiativeTestDTO.setStatus("PUBLISHED");


        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();
        beneficiaryRule.setAutomatedCriteria(new ArrayList<>());
        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeTestDTO.setBeneficiaryRule(beneficiaryRule);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        LocalDate today = LocalDate.now();
        general.setStartDate(today);
        general.setEndDate(today.plusDays(1));
        general.setRankingStartDate(today);
        general.setRankingEndDate(today.plusDays(2));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeTestDTO.setGeneral(general);


        InitiativeAdditionalDTO additional = new InitiativeAdditionalDTO();
        additional.setServiceId("serviceId");
        initiativeTestDTO.setAdditionalInfo(additional);

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeTestDTO);

        doNothing().when(onboardingService).checkDates(any(), any());
        doNothing().when(onboardingService).checkBudget(any(), any());

        when(consentMapper.map(any())).thenAnswer(invocation -> {
            Onboarding onboarding = invocation.getArgument(0);
            return OnboardingDTO.builder()
                    .userId(onboarding.getUserId())
                    .initiativeId(onboarding.getInitiativeId())
                    .status(onboarding.getStatus())
                    .build();
        });

        when(onboardingRepositoryMock.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        onboardingService.saveOnboarding(consent, userId);

        verify(onboardingRepositoryMock, times(1)).save(any(Onboarding.class));
        verify(onboardingProducer, times(1)).sendSaveConsent(any(OnboardingDTO.class));
    }

    @Test
    void testSaveOnboardingWeb_AllowsOnboarding_WhenAutomatedCriteriaPresentAndPdndAccepted() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setUserMail("test@mail.com");
        consent.setUserMailConfirmation("test@mail.com");
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);
        consent.setChannel(ChannelType.WEB);

        doReturn(null).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

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
        LocalDate today = LocalDate.now();
        general.setStartDate(today);
        general.setEndDate(today.plusDays(1));
        general.setRankingStartDate(today);
        general.setRankingEndDate(today.plusDays(2));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeTestDTO.setGeneral(general);


        InitiativeAdditionalDTO additional = new InitiativeAdditionalDTO();
        additional.setServiceId("serviceId");
        initiativeTestDTO.setAdditionalInfo(additional);

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeTestDTO);

        doNothing().when(onboardingService).checkDates(any(), any());
        doNothing().when(onboardingService).checkBudget(any(), any());
        doNothing().when(onboardingService).selfDeclaration(any(), any(), any());

        when(consentMapper.map(any(Onboarding.class))).thenAnswer(invocation -> {
            Onboarding onboarding = invocation.getArgument(0);
            return OnboardingDTO.builder()
                    .userId(onboarding.getUserId())
                    .initiativeId(onboarding.getInitiativeId())
                    .status(onboarding.getStatus())
                    .build();
        });

        when(onboardingRepositoryMock.save(any(Onboarding.class))).thenAnswer(invocation -> invocation.getArgument(0));

        onboardingService.saveOnboarding(consent, userId);

        verify(onboardingRepositoryMock, times(1)).save(any(Onboarding.class));
        verify(onboardingProducer, times(1)).sendSaveConsent(any(OnboardingDTO.class));
    }

    @Test
    void testSaveOnboardingAppIO_AllowsOnboarding_WhenAutomatedCriteriaPresentAndPdndAccepted() {
        String initiativeId = "TEST_INITIATIVE";
        String userId = "USER123";

        ConsentPutDTO consent = new ConsentPutDTO();
        consent.setInitiativeId(initiativeId);
        consent.setConfirmedTos(true);
        consent.setPdndAccept(true);
        consent.setChannel(ChannelType.APP_IO);

        doReturn(null).when(onboardingService).findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        initiativeDTO = new InitiativeDTO();
        initiativeDTO.setInitiativeId(initiativeId);
        initiativeDTO.setStatus("PUBLISHED");

        InitiativeBeneficiaryRuleDTO ruleDTO = new InitiativeBeneficiaryRuleDTO();
        ruleDTO.setAutomatedCriteria(List.of(new AutomatedCriteriaDTO()));
        ruleDTO.setSelfDeclarationCriteria(new ArrayList<>());
        initiativeDTO.setBeneficiaryRule(ruleDTO);

        InitiativeGeneralDTO general = new InitiativeGeneralDTO();
        LocalDate today = LocalDate.now();
        general.setStartDate(today);
        general.setEndDate(today.plusDays(1));
        general.setRankingStartDate(today);
        general.setRankingEndDate(today.plusDays(2));
        general.setBeneficiaryKnown(false);
        general.setBeneficiaryBudget(BigDecimal.valueOf(1000));
        initiativeDTO.setGeneral(general);


        InitiativeAdditionalDTO additional = new InitiativeAdditionalDTO();
        additional.setServiceId("serviceId");
        initiativeDTO.setAdditionalInfo(additional);

        when(initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId)).thenReturn(initiativeDTO);
        doNothing().when(onboardingService).checkDates(any(), any());
        doNothing().when(onboardingService).checkBudget(any(), any());
        doNothing().when(onboardingService).selfDeclaration(any(), any(), any());

        when(consentMapper.map(any())).thenAnswer(invocation -> {
            Onboarding onboarding = invocation.getArgument(0);
            return OnboardingDTO.builder()
                    .userId(onboarding.getUserId())
                    .initiativeId(onboarding.getInitiativeId())
                    .status(onboarding.getStatus())
                    .build();
        });

        when(onboardingRepositoryMock.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        onboardingService.saveOnboarding(consent, userId);

        verify(onboardingRepositoryMock).save(any(Onboarding.class));
        verify(onboardingProducer).sendSaveConsent(any(OnboardingDTO.class));
    }


    @Test
    void putTc_ok_OnboardingNull() {

        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(
                        Optional.empty());

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
                .thenReturn(INITIATIVE_STATUS_DTO);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setTc(true);
            onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
            onboarding.setTcAcceptTimestamp(LocalDateTime.now());
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));
        onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());

        assertEquals(INITIATIVE_ID, onboarding.getInitiativeId());
        assertEquals(USER_ID, onboarding.getUserId());
        assertEquals(OnboardingWorkflowConstants.ACCEPTED_TC, onboarding.getStatus());
        assertTrue(onboarding.getTc());
    }

    @Test
    void putTc_ko_status_not_published() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        final InitiativeStatusDTO initiativeStatusDTO = new InitiativeStatusDTO("TEST", true);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(
                        Optional.empty());

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
                .thenReturn(initiativeStatusDTO);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
            onboarding.setOnboardingKODate(LocalDateTime.now());
            onboarding.setUpdateDate(LocalDateTime.now());
            onboarding.setDetailKO(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED);
            return onboarding;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));
        try {
            onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());
            fail();
        } catch (InitiativeBudgetExhaustedException e) {
            assertEquals(ONBOARDING_BUDGET_EXHAUSTED, e.getCode());
        }

        assertEquals(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED, onboarding.getDetailKO());
        assertEquals(OnboardingWorkflowConstants.ONBOARDING_KO, onboarding.getStatus());
    }

    @Test
    void putTc_ko_budget() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        final InitiativeStatusDTO initiativeStatusDTO =
                new InitiativeStatusDTO(OnboardingWorkflowConstants.PUBLISHED, false);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(
                        Optional.empty());

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
                .thenReturn(initiativeStatusDTO);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
            onboarding.setOnboardingKODate(LocalDateTime.now());
            onboarding.setUpdateDate(LocalDateTime.now());
            onboarding.setDetailKO(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED);
            return onboarding;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));
        try {
            onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());
            fail();
        } catch (InitiativeBudgetExhaustedException e) {
            assertEquals(ONBOARDING_BUDGET_EXHAUSTED, e.getCode());
        }

        assertEquals(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED, onboarding.getDetailKO());
        assertEquals(OnboardingWorkflowConstants.ONBOARDING_KO, onboarding.getStatus());
    }

    @Test
    void putTc_ko_status_not_published_and_noBudget() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        final InitiativeStatusDTO initiativeStatusDTO = new InitiativeStatusDTO("TEST", false);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(
                        Optional.empty());

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
                .thenReturn(initiativeStatusDTO);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
            onboarding.setOnboardingKODate(LocalDateTime.now());
            onboarding.setUpdateDate(LocalDateTime.now());
            onboarding.setDetailKO(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED);
            return onboarding;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));
        try {
            onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());
            fail();
        } catch (InitiativeBudgetExhaustedException e) {
            assertEquals(ONBOARDING_BUDGET_EXHAUSTED, e.getCode());
        }

        assertEquals(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED, onboarding.getDetailKO());
        assertEquals(OnboardingWorkflowConstants.ONBOARDING_KO, onboarding.getStatus());
    }

    @Test
    void putTc_ok_invited() {

        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.INVITED);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(
                        Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_WHITELIST);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setTc(true);
            onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
            onboarding.setTcAcceptTimestamp(LocalDateTime.now());
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));
        onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());

        assertEquals(INITIATIVE_ID, onboarding.getInitiativeId());
        assertEquals(USER_ID, onboarding.getUserId());
        assertEquals(OnboardingWorkflowConstants.ACCEPTED_TC, onboarding.getStatus());
        assertTrue(onboarding.getTc());
    }

    @Test
    void putTc_ok_demanded() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.DEMANDED);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setTc(true);
            onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
            onboarding.setTcAcceptTimestamp(LocalDateTime.now());
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));
        onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());

        assertEquals(INITIATIVE_ID, onboarding.getInitiativeId());
        assertEquals(USER_ID, onboarding.getUserId());
        assertEquals(OnboardingWorkflowConstants.ACCEPTED_TC, onboarding.getStatus());
        assertTrue(onboarding.getTc());
        verify(admissibilityRestConnector, Mockito.times(0)).getInitiativeStatus(any());
    }

    @Test
    void putTc_ok_no_demanded_ranking() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(
                        Optional.empty());

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_RANKING);

        when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
                .thenReturn(INITIATIVE_STATUS_DTO);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setTc(true);
            onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
            onboarding.setTcAcceptTimestamp(LocalDateTime.now());
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));
        onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());

        assertEquals(INITIATIVE_ID, onboarding.getInitiativeId());
        assertEquals(USER_ID, onboarding.getUserId());
        assertEquals(OnboardingWorkflowConstants.ACCEPTED_TC, onboarding.getStatus());
        assertTrue(onboarding.getTc());
    }

    @Test
    void putTc_ok_demanded_ranking() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.DEMANDED);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_RANKING);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setTc(true);
            onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
            onboarding.setTcAcceptTimestamp(LocalDateTime.now());
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));
        onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());

        assertEquals(INITIATIVE_ID, onboarding.getInitiativeId());
        assertEquals(USER_ID, onboarding.getUserId());
        assertEquals(OnboardingWorkflowConstants.ACCEPTED_TC, onboarding.getStatus());
        assertTrue(onboarding.getTc());
        verify(admissibilityRestConnector, Mockito.times(0)).getInitiativeStatus(any());
    }

    @Test
    void putTc_ok_demanded_outOnboardingRange() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.DEMANDED);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        LocalDate nowLocalDate = LocalDate.now();
        InitiativeDTO initiative = initiativeDetailDTO(BENEFICIARY_TYPE_NF,
                nowLocalDate.minusDays(25),
                nowLocalDate.minusDays(20),
                nowLocalDate.minusDays(10),
                nowLocalDate.plusDays(20));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(initiative);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setTc(true);
            onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
            onboarding.setTcAcceptTimestamp(LocalDateTime.now());
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));

        onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());

        assertEquals(INITIATIVE_ID, onboarding.getInitiativeId());
        assertEquals(USER_ID, onboarding.getUserId());
        assertEquals(OnboardingWorkflowConstants.ACCEPTED_TC, onboarding.getStatus());
        assertTrue(onboarding.getTc());
        verify(admissibilityRestConnector, Mockito.times(0)).getInitiativeStatus(any());
    }

    @Test
    void putTc_ko_not_demanded_outOnboardingRange() {
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.empty());

        LocalDate nowLocalDate = LocalDate.now();
        InitiativeDTO initiative = initiativeDetailDTO(BENEFICIARY_TYPE_NF,
                nowLocalDate.minusDays(25),
                nowLocalDate.minusDays(20),
                nowLocalDate.minusDays(10),
                nowLocalDate.plusDays(20));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(initiative);

        try {
            onboardingService.putTcConsent(INITIATIVE_ID, USER_ID);
            Assertions.fail();
        } catch (InitiativeInvalidException e) {
            Assertions.assertEquals(ONBOARDING_INITIATIVE_ENDED, e.getCode());
        }

        verify(admissibilityRestConnector, Mockito.times(0)).getInitiativeStatus(any());
    }

    @Test
    void putTc_idemp() {

        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setTc(true);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTcAcceptTimestamp(LocalDateTime.now());

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(
                        Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        assertDoesNotThrow(() -> onboardingService.putTcConsent(INITIATIVE_ID, USER_ID));

    }

    @Test
    void putTC_ko_initiative_closed() {
        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_KO);
        try {
            onboardingService.putTcConsent(INITIATIVE_ID, USER_ID);
        } catch (InitiativeInvalidException e) {
            assertEquals(ONBOARDING_INITIATIVE_STATUS_NOT_PUBLISHED, e.getCode());
        }
    }

    @Test
    void putTC_ko_unsubscribed() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(
                        Optional.of(onboarding));
        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);
        try {
            onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());
        } catch (UserUnsubscribedException e) {
            assertEquals(ONBOARDING_USER_UNSUBSCRIBED, e.getCode());
            assertEquals(String.format(ERROR_UNSUBSCRIBED_INITIATIVE_MSG, onboarding.getInitiativeId()), e.getMessage());
        }
    }

    @Test
    void putTC_onboardingKO() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setDetailKO(OnboardingWorkflowConstants.ERROR_INITIATIVE_END);
        onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(
                        Optional.of(onboarding));
        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);
        doThrow(new InitiativeInvalidException(ONBOARDING_INITIATIVE_ENDED, String.format(ERROR_INITIATIVE_END_MSG, INITIATIVE_ID)))
                .when(utilities).throwOnboardingKOException(anyString(), anyString());

        try {
            onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());
        } catch (InitiativeInvalidException e) {
            assertEquals(ONBOARDING_INITIATIVE_ENDED, e.getCode());
        }
    }

    //endregion     DA QUI

    @Test
    void getOnboardingStatus_ok() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
        onboarding.setUpdateDate(LocalDateTime.now());
        onboarding.setOnboardingOkDate(LocalDateTime.now());

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        OnboardingStatusDTO onboardingStatusDTO = onboardingService.getOnboardingStatus(INITIATIVE_ID, USER_ID);

        assertEquals(onboarding.getStatus(), onboardingStatusDTO.getStatus());
        assertEquals(onboarding.getUpdateDate(), onboardingStatusDTO.getStatusDate());
        assertEquals(onboarding.getOnboardingOkDate(), onboardingStatusDTO.getOnboardingOkDate());

    }

    @Test
    void getOnboardingStatus_ko() {
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.empty());
        try {
            onboardingService.getOnboardingStatus(INITIATIVE_ID, USER_ID);
        } catch (UserNotOnboardedException e) {
            assertEquals(USER_NOT_ONBOARDED, e.getCode());
            assertEquals(String.format(ID_S_NOT_FOUND_MSG, INITIATIVE_ID), e.getMessage());
        }

    }

    @Test
    void getOnboardingStatus_nullOnboardingOkDate() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setUpdateDate(LocalDateTime.now());

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        OnboardingStatusDTO onboardingStatusDTO = onboardingService.getOnboardingStatus(INITIATIVE_ID, USER_ID);

        assertEquals(onboarding.getStatus(), onboardingStatusDTO.getStatus());
        assertEquals(onboarding.getUpdateDate(), onboardingStatusDTO.getStatusDate());
        assertNull(onboardingStatusDTO.getOnboardingOkDate());

    }

    //region Pre-Requisites case test
    @Test
    void checkPrerequisites_ok_no_whitelist() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
                .thenReturn(INITIATIVE_STATUS_DTO);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setChannel(CHANNEL);
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));

        assertDoesNotThrow(() -> onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID,
                CHANNEL));
    }

    @Test
    void checkPrerequisites_ok_no_whitelist_no_pdnd() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_NO_PDND);

        when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
                .thenReturn(INITIATIVE_STATUS_DTO);

        assertDoesNotThrow(() -> onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID,
                CHANNEL));
    }

    @Test
    void checkPrerequisites_ok_no_whitelist_no_self() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_NO_SELF);

        when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
                .thenReturn(INITIATIVE_STATUS_DTO);

        assertDoesNotThrow(() -> onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID,
                CHANNEL));
    }

    @Test
    void checkPrerequisites_ok_whitelist() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);
        onboarding.setInvitationDate(LocalDateTime.now());

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_WHITELIST);
        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setChannel(CHANNEL);
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));

        assertDoesNotThrow(() -> onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL));
    }

    @Test
    void checkPrerequisites_ko_whitelist() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_WHITELIST);

        try {
            onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL);
            Assertions.fail();
        } catch (UserNotInWhitelistException e) {
            assertEquals(USER_NOT_IN_WHITELIST, e.getCode());
            assertEquals(String.format(ERROR_WHITELIST_MSG, onboarding.getInitiativeId()), e.getMessage());
        }

    }

    @Test
    void checkPrerequisites_ko_start_date() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);
        INITIATIVE_DTO_KO_START_DATE.setInitiativeId(INITIATIVE_ID);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_KO_START_DATE);

        try {
            onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL);
        } catch (InitiativeInvalidException e) {
            assertEquals(INITIATIVE_NOT_STARTED, e.getCode());
            assertEquals(String.format(ERROR_INITIATIVE_NOT_STARTED_MSG, onboarding.getInitiativeId()), e.getMessage());
        }
    }

    @Test
    void checkPrerequisites_ko_ranking_start_date() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);
        INITIATIVE_DTO_KO_RANKING_START_DATE.setInitiativeId(INITIATIVE_ID);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_KO_RANKING_START_DATE);

        try {
            onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL);
        } catch (InitiativeInvalidException e) {
            assertEquals(INITIATIVE_NOT_STARTED, e.getCode());
            assertEquals(String.format(ERROR_INITIATIVE_NOT_STARTED_MSG, onboarding.getInitiativeId()), e.getMessage());
        }
    }

    @Test
    void checkPrerequisites_ko_end_date() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);
        INITIATIVE_DTO_KO_END_DATE.setInitiativeId(INITIATIVE_ID);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_KO_END_DATE);

        try {
            onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL);
        } catch (InitiativeInvalidException e) {
            assertEquals(ONBOARDING_INITIATIVE_ENDED, e.getCode());
            assertEquals(String.format(ERROR_INITIATIVE_END_MSG, onboarding.getInitiativeId()), e.getMessage());
        }
    }

    @Test
    void checkPrerequisites_ko_ranking_end_date() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);
        INITIATIVE_DTO_KO_RANKING_END_DATE.setInitiativeId(INITIATIVE_ID);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_KO_RANKING_END_DATE);

        try {
            onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL);
        } catch (InitiativeInvalidException e) {
            assertEquals(ONBOARDING_INITIATIVE_ENDED, e.getCode());
            assertEquals(String.format(ERROR_INITIATIVE_END_MSG, onboarding.getInitiativeId()), e.getMessage());
        }
    }

    @Test
    void checkPrerequisites_ko_budget_terminated() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
                .thenReturn(INITIATIVE_NOT_BUDGET_DTO);

        try {
            onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL);
            Assertions.fail();
        } catch (InitiativeBudgetExhaustedException e) {
            Assertions.assertEquals(ONBOARDING_BUDGET_EXHAUSTED, e.getCode());
            Assertions.assertEquals(String.format(ERROR_BUDGET_TERMINATED_MSG, INITIATIVE_ID), e.getMessage());
        }
    }

    @Test
    void checkPrerequisites_ko_initiative_closed() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);

        initiativeDTO.setInitiativeId(INITIATIVE_ID);
        initiativeDTO.setStatus("CLOSED");

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(initiativeDTO);

        InitiativeInvalidException e = assertThrows(
                InitiativeInvalidException.class,
                () -> onboardingService.checkPrerequisites(onboarding.getInitiativeId(), onboarding.getUserId(), CHANNEL)
        );

        assertEquals(ONBOARDING_INITIATIVE_STATUS_NOT_PUBLISHED, e.getCode());
        assertEquals(
                String.format(ERROR_INITIATIVE_NOT_ACTIVE_MSG, onboarding.getInitiativeId()),
                e.getMessage()
        );
    }


    @Test
    void checkPrerequisites_ko_whitelist_fail() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(
                        Optional.of(onboarding));
        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_WHITELIST);
        try {
            onboardingService.checkPrerequisites(onboarding.getInitiativeId(), onboarding.getUserId(),
                    CHANNEL);
        } catch (UserNotInWhitelistException e) {
            assertEquals(USER_NOT_IN_WHITELIST, e.getCode());
            assertEquals(String.format(ERROR_WHITELIST_MSG, onboarding.getInitiativeId()), e.getMessage());
        }
    }

    @Test
    void checkPrerequisites_onboardingKO() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
        onboarding.setDetailKO(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED);

        doThrow(new InitiativeBudgetExhaustedException(String.format(ERROR_BUDGET_TERMINATED_MSG, INITIATIVE_ID)))
                .when(utilities).throwOnboardingKOException(anyString(), anyString());

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        try {
            onboardingService.checkPrerequisites(onboarding.getInitiativeId(), onboarding.getUserId(), CHANNEL);
        } catch (InitiativeBudgetExhaustedException e) {
            assertEquals(ONBOARDING_BUDGET_EXHAUSTED, e.getCode());
        }
    }

    @Test
    void checkPrerequisites_onboardingKO_birthdateKO() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
        onboarding.setDetailKO(OnboardingWorkflowConstants.REJECTION_REASON_BIRTHDATE_KO);
        Mockito.when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        RequiredCriteriaDTO res = onboardingService.checkPrerequisites(onboarding.getInitiativeId(), onboarding.getUserId(), CHANNEL);
        assertEquals(INITIATIVE_BENEFICIARY_RULE_DTO.getSelfDeclarationCriteria(), res.getSelfDeclarationList());
    }

    @Test
    void checkPrerequisites_onboardingOK_noWhitelist() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        RequiredCriteriaDTO res = onboardingService.checkPrerequisites(onboarding.getInitiativeId(), onboarding.getUserId(), CHANNEL);
        assertEquals(INITIATIVE_BENEFICIARY_RULE_DTO.getSelfDeclarationCriteria(), res.getSelfDeclarationList());
    }

    @Test
    void checkPrerequisites_onboardingOK_ranking() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_RANKING);

        RequiredCriteriaDTO res = onboardingService.checkPrerequisites(onboarding.getInitiativeId(), onboarding.getUserId(), CHANNEL);
        assertEquals(INITIATIVE_BENEFICIARY_RULE_DTO.getSelfDeclarationCriteria(), res.getSelfDeclarationList());
    }

    @Test
    void checkPrerequisites_onboardingOK_whitelist() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_WHITELIST);

        RequiredCriteriaDTO res = onboardingService.checkPrerequisites(onboarding.getInitiativeId(), onboarding.getUserId(), CHANNEL);
        assertNull(res);
    }

    @Test
    void checkPrerequisites_ok_familyUnit() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);
        onboarding.setDemandedDate(LocalDateTime.now());
        INITIATIVE_DTO.getGeneral().setBeneficiaryType(BENEFICIARY_TYPE_NF);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setChannel(CHANNEL);
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));

        assertDoesNotThrow(() -> onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL));
    }

    @Test
    void checkPrerequisites_ok_familyUnit_ranking() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);
        onboarding.setDemandedDate(LocalDateTime.now());
        INITIATIVE_DTO.getGeneral().setBeneficiaryType(BENEFICIARY_TYPE_NF);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(INITIATIVE_DTO_RANKING);

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setChannel(CHANNEL);
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));

        assertDoesNotThrow(() -> onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL));

    }

    @Test
    void checkPrerequisites_ok_demanded_outOnboardingRange() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);
        onboarding.setDemandedDate(LocalDateTime.now());

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        LocalDate localDateNow = LocalDate.now();
        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(initiativeDetailDTO(BENEFICIARY_TYPE_NF,
                        localDateNow.minusDays(30),
                        localDateNow.minusDays(20),
                        localDateNow.plusDays(2),
                        localDateNow.plusDays(25)));

        Mockito.doAnswer(invocationOnMock -> {
            onboarding.setChannel(CHANNEL);
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));

        assertDoesNotThrow(() -> onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL));
    }

    @Test
    void checkPrerequisites_KO_Notdemanded_outOnboardingRange_familyInitiative() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setTc(true);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        LocalDate localDateNow = LocalDate.now();
        when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
                .thenReturn(initiativeDetailDTO(BENEFICIARY_TYPE_NF,
                        localDateNow.minusDays(30),
                        localDateNow.minusDays(20),
                        localDateNow.plusDays(2),
                        localDateNow.plusDays(25)));

        try {
            onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL);
            Assertions.fail();
        } catch (InitiativeInvalidException e) {
            Assertions.assertEquals(ONBOARDING_INITIATIVE_ENDED, e.getCode());
        }
    }

    //endregion


    @Test
    void completeOnboarding_ok() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        onboardingService.completeOnboarding(EVALUATION_DTO);
        assertEquals(OnboardingWorkflowConstants.ONBOARDING_OK, onboarding.getStatus());
    }

    @Test
    void completeOnboardingDEMANDEDWithOnboardingNotNull() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus("DEMANDED");
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        EVALUATION_DTO.setStatus("DEMANDED");
        onboardingService.completeOnboarding(EVALUATION_DTO);
        assertEquals(OnboardingWorkflowConstants.ONBOARDING_OK, onboarding.getStatus());
    }

    @Test
    void completeOnboarding_noOnboardingFound() {
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.empty());
        onboardingService.completeOnboarding(EVALUATION_DTO);
        verify(onboardingRepositoryMock, Mockito.times(1))
                .findById(Onboarding.buildId(INITIATIVE_ID, USER_ID));
    }

    @Test
    void completeOnboarding_ko_eligible_ko() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        onboardingService.completeOnboarding(EVALUATION_DTO_ONBOARDING_KO_OUT_OF_RANKING);
        assertEquals(OnboardingWorkflowConstants.ELIGIBLE_KO, onboarding.getStatus());
        assertEquals("CITIZEN_OUT_OF_RANKING" + ',' + INVALID_INITIATIVE, onboarding.getDetailKO());
    }

    @Test
    void completeOnboarding_ko_no_onb() {
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.empty());
        onboardingService.completeOnboarding(EVALUATION_DTO_ONBOARDING_KO);
        verify(onboardingRepositoryMock, Mockito.times(1))
                .findById(Onboarding.buildId(INITIATIVE_ID, USER_ID));
    }

    @Test
    void completeOnboarding_ko_no_onb_eligible_ko() {
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.empty());
        onboardingService.completeOnboarding(EVALUATION_DTO_ONBOARDING_KO_OUT_OF_RANKING);
        verify(onboardingRepositoryMock, Mockito.times(1))
                .findById(Onboarding.buildId(INITIATIVE_ID, USER_ID));
    }

    @Test
    void completeOnboardingCreateOnboardingStatusDEMANDED_ok() {
        EVALUATION_DTO.setStatus("DEMANDED");
        assertDoesNotThrow(() -> onboardingService.completeOnboarding(EVALUATION_DTO));
    }

    @Test
    void completeOnboarding_genericError_rejectionReason() {
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.empty());
        EVALUATION_DTO.setOnboardingRejectionReasons(List.of(new OnboardingRejectionReason
                (INVALID_INITIATIVE, OnboardingWorkflowConstants.GENERIC_ERROR, null, null, null)));
        onboardingService.completeOnboarding(EVALUATION_DTO_ONBOARDING_KO_OUT_OF_RANKING);
        verify(onboardingRepositoryMock, Mockito.times(1))
                .findById(Onboarding.buildId(INITIATIVE_ID, USER_ID));
    }

    @Test
    void checkChangeJOINEDStatusInToONBOARDING_OK() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        EVALUATION_DTO.setStatus("JOINED");

        onboardingService.completeOnboarding(EVALUATION_DTO);

        assertEquals(OnboardingWorkflowConstants.ONBOARDING_OK, onboarding.getStatus());

    }

    @Test
    void checkChangeREJECTEDtatusInToONBOARDING_KO() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        EVALUATION_DTO.setStatus("REJECTED");

        onboardingService.completeOnboarding(EVALUATION_DTO);

        assertEquals(OnboardingWorkflowConstants.ONBOARDING_KO, onboarding.getStatus());

    }

    @Test
    void deactivateOnboarding_ok() {

        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        Mockito.doAnswer(
                        invocationOnMock -> {
                            onboarding.setRequestDeactivationDate(LocalDateTime.now());
                            onboarding.setStatus(OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED);
                            return null;
                        })
                .when(onboardingRepositoryMock).save(any(Onboarding.class));
        onboardingService.deactivateOnboarding(INITIATIVE_ID, USER_ID, LocalDateTime.now().toString());
        assertNotNull(onboarding.getRequestDeactivationDate());
        assertEquals(OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED, onboarding.getStatus());
    }

    @Test
    void deactivateOnboarding_ko() {
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.empty());
        String date = LocalDateTime.now().toString();
        try {
            onboardingService.deactivateOnboarding(INITIATIVE_ID, USER_ID, date);
            Assertions.fail();
        } catch (UserNotOnboardedException e) {
            assertEquals(USER_NOT_ONBOARDED, e.getCode());
            assertEquals(String.format(ID_S_NOT_FOUND_MSG, INITIATIVE_ID), e.getMessage());
        }
    }

    @Test
    void rollback() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        onboardingService.rollback(INITIATIVE_ID, USER_ID);
        assertNull(onboarding.getRequestDeactivationDate());
        assertEquals(OnboardingWorkflowConstants.ONBOARDING_OK, onboarding.getStatus());
    }

    @Test
    void rollback_null() {
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.empty());
        onboardingService.rollback(INITIATIVE_ID, USER_ID);
        verify(onboardingRepositoryMock, Mockito.times(0)).save(any());
    }

    @Test
    void rollback_status_ko() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        onboardingService.rollback(INITIATIVE_ID, USER_ID);
        verify(onboardingRepositoryMock, Mockito.times(0)).save(any());
    }

    @Test
    void getOnboardingStatusList_ok() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setUpdateDate(LocalDateTime.now());
        List<Onboarding> onboardingList = List.of(onboarding);
        Criteria criteria = new Criteria();
        Long count = 15L;
        Pageable paging = PageRequest.of(0, 15, Sort.by("lastUpdate"));

        when(
                onboardingRepositoryMock.getCriteria(INITIATIVE_ID, USER_ID, STATUS, START_DATE, END_DATE))
                .thenReturn(criteria);

        when(onboardingRepositoryMock.findByFilter(criteria, paging))
                .thenReturn(onboardingList);

        when(onboardingRepositoryMock.getCount(criteria)).thenReturn(count);

        assertDoesNotThrow(() -> onboardingService.getOnboardingStatusList(INITIATIVE_ID, USER_ID, START_DATE, END_DATE,
                STATUS,
                paging));
    }

    @Test
    void getOnboardingStatusList_ok_page_null() {
        final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        List<Onboarding> onboardingList = List.of(onboarding);
        Criteria criteria = new Criteria();
        Long count = 15L;

        when(
                onboardingRepositoryMock.getCriteria(INITIATIVE_ID, USER_ID, STATUS, START_DATE, END_DATE))
                .thenReturn(criteria);

        when(onboardingRepositoryMock.findByFilter(criteria, null))
                .thenReturn(onboardingList);

        when(onboardingRepositoryMock.getCount(criteria)).thenReturn(count);

        assertDoesNotThrow(() -> onboardingService.getOnboardingStatusList(INITIATIVE_ID, USER_ID, START_DATE, END_DATE,
                STATUS,
                null));
    }

    @Test
    void getOnboardingStatusList_ko() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboarding.setUpdateDate(LocalDateTime.now());
        Pageable paging = PageRequest.of(0, 20, Sort.by("lastUpdate"));

        try {
            onboardingService.getOnboardingStatusList(INITIATIVE_ID, USER_ID, START_DATE, END_DATE,
                    STATUS, paging);
            fail();
        } catch (PageSizeNotAllowedException e) {
            assertEquals(PAGE_SIZE_NOT_ALLOWED, e.getCode());
            assertEquals(String.format(ERROR_MAX_NUMBER_FOR_PAGE_MSG), e.getMessage());
        }
    }

    @Test
    void allowedInitiative_ok() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        assertDoesNotThrow(() -> onboardingService.allowedInitiative(ONBOARDING_NOTIFICATION_DTO));
    }

    @Test
    void allowedInitiative_ignore() {
        assertDoesNotThrow(() -> onboardingService.allowedInitiative(ONBOARDING_NOTIFICATION_DTO_IBAN));
    }


    @Test
    void allowedInitiative_ok_null() {
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.empty());

        Mockito.doAnswer(invocation -> {
            Onboarding saved = invocation.getArgument(0, Onboarding.class);
            saved.setStatus("CREATED"); // qui metti un valore vero, non anyString()
            saved.setInvitationDate(LocalDateTime.now());
            saved.setUpdateDate(LocalDateTime.now());
            saved.setCreationDate(LocalDateTime.now());
            return null;
        }).when(onboardingRepositoryMock).save(any(Onboarding.class));

        assertDoesNotThrow(() -> onboardingService.allowedInitiative(ONBOARDING_NOTIFICATION_DTO));
    }


    @Test
    void suspend_ok() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        onboardingService.suspend(INITIATIVE_ID, USER_ID);
        assertEquals(OnboardingWorkflowConstants.SUSPENDED, onboarding.getStatus());
    }

    @Test
    void suspend_wrongStatus() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        try {
            onboardingService.suspend(INITIATIVE_ID, USER_ID);
        } catch (OperationNotAllowedException e) {
            assertEquals(SUSPENSION_NOT_ALLOWED, e.getCode());
            assertEquals(String.format(ERROR_SUSPENSION_STATUS_MSG, onboarding.getInitiativeId()), e.getMessage());
        }
    }

    @Test
    void suspend_ko() {
        String mongoFullErrorResponse = """
                {"ok": 0.0, "errmsg": "Error=16500, RetryAfterMs=34,\s
                Details='Response status code does not indicate success: TooManyRequests (429) Substatus: 3200 ActivityId: 46ba3855-bc3b-4670-8609-17e1c2c87778 Reason:\s
                (\\r\\nErrors : [\\r\\n \\"Request rate is large. More Request Units may be needed, so no changes were made. Please retry this request later. Learn more:
                 http://aka.ms/cosmosdb-error-429\\"\\r\\n]\\r\\n) ", "code": 16500, "codeName": "RequestRateTooLarge"}
                """;

        final MongoQueryException mongoQueryException = new MongoQueryException(
                BsonDocument.parse(mongoFullErrorResponse), new ServerAddress());

        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        Mockito.doThrow(new UncategorizedMongoDbException(mongoQueryException.getMessage(), mongoQueryException)).when(onboardingRepositoryMock).save(any());

        try {
            onboardingService.suspend(INITIATIVE_ID, USER_ID);
        } catch (UserSuspensionOrReadmissionException e) {
            assertEquals(GENERIC_ERROR, e.getCode());
            assertEquals(String.format(ERROR_SUSPENSION_MSG, onboarding.getInitiativeId()), e.getMessage());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {OnboardingWorkflowConstants.SUSPENDED, OnboardingWorkflowConstants.ONBOARDING_OK})
    void readmit_ok(String status) {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(status);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        onboardingService.readmit(INITIATIVE_ID, USER_ID);
        assertEquals(OnboardingWorkflowConstants.ONBOARDING_OK, onboarding.getStatus());
    }

    @ParameterizedTest
    @ValueSource(strings = {OnboardingWorkflowConstants.ON_EVALUATION, OnboardingWorkflowConstants.INVITED, OnboardingWorkflowConstants.ACCEPTED_TC, OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED, OnboardingWorkflowConstants.ONBOARDING_KO})
    void readmit_wrongStatus(String status) {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(status);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));
        try {
            onboardingService.readmit(INITIATIVE_ID, USER_ID);
        } catch (OperationNotAllowedException e) {
            assertEquals(READMISSION_NOT_ALLOWED, e.getCode());
            assertEquals(String.format(ERROR_READMIT_STATUS_MSG, onboarding.getInitiativeId()), e.getMessage());
        }
    }

    @Test
    void readmit_ko() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(OnboardingWorkflowConstants.SUSPENDED);
        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboarding));

        Mockito.doThrow(new MongoException(500, "")).when(onboardingRepositoryMock).save(any());

        try {
            onboardingService.readmit(INITIATIVE_ID, USER_ID);
        } catch (UserSuspensionOrReadmissionException e) {
            assertEquals(GENERIC_ERROR, e.getCode());
            assertEquals(String.format(ERROR_READMISSION_MSG, onboarding.getInitiativeId()), e.getMessage());
        }
    }

    @Test
    void getFamilyUnitComposition_ok() {
        final Onboarding onboardingOk = new Onboarding(INITIATIVE_ID, USER_ID);
        onboardingOk.setFamilyId(FAMILY_ID);
        onboardingOk.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
        onboardingOk.setOnboardingOkDate(LocalDateTime.now().minusDays(2));

        final Onboarding onboardingDemanded = new Onboarding(INITIATIVE_ID, "USER_ID_2");
        onboardingDemanded.setFamilyId(FAMILY_ID);
        onboardingOk.setStatus("DEMANDED");

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboardingOk));
        when(onboardingRepositoryMock.findByInitiativeIdAndFamilyId(INITIATIVE_ID, FAMILY_ID))
                .thenReturn(List.of(onboardingOk, onboardingDemanded));

        when(decryptRestConnector.getPiiByToken(USER_ID))
                .thenReturn(new DecryptCfDTO(PII));
        when(decryptRestConnector.getPiiByToken("USER_ID_2"))
                .thenReturn(new DecryptCfDTO("PII_2"));

        OnboardingFamilyDTO onboardingFamilyDTO = onboardingService.getfamilyUnitComposition(INITIATIVE_ID, USER_ID);

        assertEquals(PII, onboardingFamilyDTO.getUsersList().get(0).getFiscalCode());
        assertEquals(onboardingOk.getFamilyId(), onboardingFamilyDTO.getUsersList().get(0).getFamilyId());
        assertEquals(onboardingOk.getOnboardingOkDate().toLocalDate(), onboardingFamilyDTO.getUsersList().get(0).getOnboardingDate());
        assertEquals(onboardingOk.getStatus(), onboardingFamilyDTO.getUsersList().get(0).getStatus());

        assertEquals("PII_2", onboardingFamilyDTO.getUsersList().get(1).getFiscalCode());
        assertEquals(onboardingDemanded.getFamilyId(), onboardingFamilyDTO.getUsersList().get(1).getFamilyId());
        assertNull(onboardingFamilyDTO.getUsersList().get(1).getOnboardingDate());
        assertEquals(onboardingDemanded.getStatus(), onboardingFamilyDTO.getUsersList().get(1).getStatus());

    }

    @Test
    void getFamilyUnitComposition_ok_onboardingKo() {
        final Onboarding onboardingKo = new Onboarding(INITIATIVE_ID, USER_ID);
        onboardingKo.setFamilyId(FAMILY_ID);
        onboardingKo.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
        onboardingKo.setOnboardingKODate(LocalDateTime.now());

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboardingKo));
        when(onboardingRepositoryMock.findByInitiativeIdAndFamilyId(INITIATIVE_ID, FAMILY_ID))
                .thenReturn(List.of(onboardingKo));

        when(decryptRestConnector.getPiiByToken(USER_ID))
                .thenReturn(new DecryptCfDTO(PII));

        OnboardingFamilyDTO onboardingFamilyDTO = onboardingService.getfamilyUnitComposition(INITIATIVE_ID, USER_ID);

        assertEquals(PII, onboardingFamilyDTO.getUsersList().get(0).getFiscalCode());
        assertEquals(onboardingKo.getFamilyId(), onboardingFamilyDTO.getUsersList().get(0).getFamilyId());
        assertEquals(onboardingKo.getOnboardingKODate().toLocalDate(), onboardingFamilyDTO.getUsersList().get(0).getOnboardingDate());
        assertEquals(onboardingKo.getStatus(), onboardingFamilyDTO.getUsersList().get(0).getStatus());

    }

    @Test
    void getFamilyUnitComposition_ok_noFamilyId() {
        final Onboarding onboardingKo = new Onboarding(INITIATIVE_ID, USER_ID);
        onboardingKo.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
        onboardingKo.setOnboardingKODate(LocalDateTime.now());

        List<OnboardingFamilyDetailDTO> usersList = new ArrayList<>();
        OnboardingFamilyDTO onboardingFamilyExpected = new OnboardingFamilyDTO(usersList);

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboardingKo));

        OnboardingFamilyDTO onboardingFamilyDTO = onboardingService.getfamilyUnitComposition(INITIATIVE_ID, USER_ID);

        assertEquals(onboardingFamilyExpected, onboardingFamilyDTO);

    }

    @Test
    void getFamilyUnitComposition_ko() {
        final Onboarding onboardingOk = new Onboarding(INITIATIVE_ID, USER_ID);
        onboardingOk.setFamilyId(FAMILY_ID);
        onboardingOk.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
        onboardingOk.setOnboardingOkDate(LocalDateTime.now().minusDays(2));

        when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
                .thenReturn(Optional.of(onboardingOk));
        when(onboardingRepositoryMock.findByInitiativeIdAndFamilyId(INITIATIVE_ID, FAMILY_ID))
                .thenReturn(List.of(onboardingOk));

        Request request =
                Request.create(
                        Request.HttpMethod.GET, "url", new HashMap<>(), null, new RequestTemplate());
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("x-api-key", Collections.singleton("x-api-key"));
        Mockito.doThrow(new FeignException.InternalServerError("", request, new byte[0], headers)).when(decryptRestConnector).getPiiByToken((USER_ID));

        try {
            onboardingService.getfamilyUnitComposition(INITIATIVE_ID, USER_ID);
        } catch (PDVInvocationException e) {
            assertEquals(GENERIC_ERROR, e.getCode());
        }
    }

    @ParameterizedTest
    @MethodSource("operationTypeAndInvocationTimes")
    void processOperation_deleteOperation(String operationType, int times) {

        QueueCommandOperationDTO queueCommandOperationDTO = QueueCommandOperationDTO.builder()
                .entityId(INITIATIVE_ID)
                .operationType(operationType)
                .operationTime(LocalDateTime.now())
                .build();

        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);

        List<Onboarding> deletedOnboardingPage = List.of(onboarding);

        if (times == 2) {
            List<Onboarding> onboardingPage = createOnboardingPage(PAGE_SIZE);

            when(onboardingRepositoryMock.deletePaged(queueCommandOperationDTO.getEntityId(), PAGE_SIZE))
                    .thenReturn(onboardingPage)
                    .thenReturn(deletedOnboardingPage);

            Thread.currentThread().interrupt();
        }


        onboardingService.processCommand(queueCommandOperationDTO);


        verify(onboardingRepositoryMock, Mockito.times(times)).deletePaged(queueCommandOperationDTO.getEntityId(), PAGE_SIZE);
    }

    private static Stream<Arguments> operationTypeAndInvocationTimes() {
        return Stream.of(
                Arguments.of(OPERATION_TYPE_DELETE_INITIATIVE, 1),
                Arguments.of(OPERATION_TYPE_DELETE_INITIATIVE, 2),
                Arguments.of("OPERATION_TYPE_TEST", 0)
        );
    }

    private List<Onboarding> createOnboardingPage(int pageSize) {
        List<Onboarding> onboardingPage = new ArrayList<>();

        for (int i = 0; i < pageSize; i++) {
            Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
            onboarding.setId("ID_ONBOARDING" + i);
            onboardingPage.add(onboarding);
        }

        return onboardingPage;
    }

    private InitiativeDTO initiativeDetailDTO(String beneficiaryType,
                                              LocalDate startRankingDate,
                                              LocalDate endRankingDate,
                                              @NotNull LocalDate startDate,
                                              @NotNull LocalDate endDate) {
        InitiativeDTO initiative = new InitiativeDTO();
        InitiativeGeneralDTO general = new InitiativeGeneralDTO();

        general.setBeneficiaryKnown(false);
        if (startRankingDate != null) {
            general.setRankingStartDate(startRankingDate);
        }
        if (endRankingDate != null) {
            general.setRankingEndDate(endRankingDate);
        }
        general.setStartDate(startDate);
        general.setEndDate(endDate);
        general.setBudget(BUDGET);
        general.setBeneficiaryBudget(BENEFICIARY_BUDGET);
        general.setRankingEnabled(Boolean.FALSE);
        general.setBeneficiaryType(beneficiaryType);

        initiative.setInitiativeId(INITIATIVE_ID);
        initiative.setStatus("PUBLISHED");
        initiative.setGeneral(general);
        initiative.setBeneficiaryRule(INITIATIVE_BENEFICIARY_RULE_DTO);
        initiative.setInitiativeRewardType(INITIATIVE_REWARD_TYPE_DISCOUNT);
        return initiative;
    }

    @Test
    void testFindOnboardingByInitiativeIdAndUserId_ReturnsOnboarding_WhenFound() {
        String initiativeId = "INIT123";
        String userId = "USER456";
        String onboardingId = Onboarding.buildId(initiativeId, userId);

        Onboarding expectedOnboarding = new Onboarding(initiativeId, userId);

        when(onboardingRepositoryMock.findById(onboardingId)).thenReturn(Optional.of(expectedOnboarding));

        Onboarding result = onboardingService.findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        assertNotNull(result);
        assertEquals(expectedOnboarding, result);
        verify(onboardingRepositoryMock, times(1)).findById(onboardingId);
    }

    @Test
    void testFindOnboardingByInitiativeIdAndUserId_ReturnsNull_WhenNotFound() {
        String initiativeId = "INIT123";
        String userId = "USER456";
        String onboardingId = Onboarding.buildId(initiativeId, userId);

        when(onboardingRepositoryMock.findById(onboardingId)).thenReturn(Optional.empty());

        Onboarding result = onboardingService.findOnboardingByInitiativeIdAndUserId(initiativeId, userId);

        assertNull(result);
        verify(onboardingRepositoryMock, times(1)).findById(onboardingId);
    }

    @Test
    void testSelfDeclaration_WhenCriteriaPresent_ShouldProcessAndSave() {
        ConsentPutDTO consentPutDTO = new ConsentPutDTO();
        SelfConsentBoolDTO selfDecl = new SelfConsentBoolDTO();
        selfDecl.setCode(SelfCriteriaBooleanTypeCode.ISEE.name());
        selfDecl.setAccepted(true);
        consentPutDTO.setSelfDeclarationList(List.of(selfDecl));

        String userId = "USER123";

        onboardingService.selfDeclaration(initiativeDTO, consentPutDTO, userId);

        verify(selfDeclarationRepository, never()).save(any(SelfDeclaration.class));
    }

    @Test
    void testSelfDeclaration_SizeMismatch_ShouldThrowException() {
        InitiativeBeneficiaryRuleDTO rule = new InitiativeBeneficiaryRuleDTO();
        rule.setSelfDeclarationCriteria(List.of(
                new SelfCriteriaBooleanTypeDTO(
                        "boolean_type", "desc", "subdesc", true,
                        SelfCriteriaBooleanTypeCode.ISEE
                )
        ));
        initiativeDTO.setBeneficiaryRule(rule);

        ConsentPutDTO consentPutDTO = new ConsentPutDTO();
        consentPutDTO.setSelfDeclarationList(List.of());

        assertThrows(SelfDeclarationCrtieriaException.class,
                () -> onboardingService.selfDeclaration(initiativeDTO, consentPutDTO, "USER123"));

        verify(auditUtilities).logOnboardingKOInitiativeId(
                eq(initiativeDTO.getInitiativeId()),
                eq(OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_SIZE_AUDIT)
        );
    }

    @Test
    void testSelfDeclaration_BoolCriteriaNull_ShouldThrowException() {
        InitiativeBeneficiaryRuleDTO rule = new InitiativeBeneficiaryRuleDTO();
        SelfCriteriaBoolDTO boolCriteria = new SelfCriteriaBoolDTO();
        boolCriteria.setCode("ISEE");
        rule.setSelfDeclarationCriteria(List.of(boolCriteria));
        initiativeDTO.setBeneficiaryRule(rule);

        ConsentPutDTO consentPutDTO = new ConsentPutDTO();
        SelfConsentBoolDTO selfDecl = new SelfConsentBoolDTO();
        selfDecl.setCode("ISEE");
        consentPutDTO.setSelfDeclarationList(List.of(selfDecl));

        assertThrows(SelfDeclarationCrtieriaException.class,
                () -> onboardingService.selfDeclaration(initiativeDTO, consentPutDTO, "USER123"));

        verify(auditUtilities).logOnboardingKOInitiativeId(
                eq(initiativeDTO.getInitiativeId()),
                eq(OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY_AUDIT)
        );
    }

    @Test
    void testSelfDeclaration_BoolCriteriaAccepted_ShouldSetValueWithoutException() {
        SelfCriteriaBoolDTO boolCriteria = new SelfCriteriaBoolDTO();
        boolCriteria.setCode("ISEE");
        initiativeDTO.getBeneficiaryRule().setSelfDeclarationCriteria(List.of(boolCriteria));

        SelfConsentBoolDTO consentBool = new SelfConsentBoolDTO();
        consentBool.setCode("ISEE");
        consentBool.setAccepted(true);
        ConsentPutDTO consentPutDTO = new ConsentPutDTO();
        consentPutDTO.setSelfDeclarationList(List.of(consentBool));

        onboardingService.selfDeclaration(initiativeDTO, consentPutDTO, "USER123");

        assertTrue(boolCriteria.getValue());
        verify(auditUtilities, never()).logOnboardingKOInitiativeId(any(), any());
        verify(selfDeclarationRepository, never()).save(any());
    }

    @Test
    void testSelfDeclaration_MultiCriteria_ShouldCallMultiCheckAndSave() {
        SelfCriteriaMultiDTO multiCriteria = new SelfCriteriaMultiDTO("multi", "desc", List.of("Value1", "Value2"), "CODE_MULTI");
        initiativeDTO.getBeneficiaryRule().setSelfDeclarationCriteria(List.of(multiCriteria));

        SelfConsentMultiDTO consentMulti = new SelfConsentMultiDTO();
        consentMulti.setCode("CODE_MULTI");
        consentMulti.setValue("Value1");
        ConsentPutDTO consentPutDTO = new ConsentPutDTO();
        consentPutDTO.setSelfDeclarationList(List.of(consentMulti));

        doNothing().when(onboardingService).multiCriteriaCheck(eq(initiativeDTO), eq(multiCriteria), anyMap());

        onboardingService.selfDeclaration(initiativeDTO, consentPutDTO, "USER123");

        verify(onboardingService).multiCriteriaCheck(eq(initiativeDTO), eq(multiCriteria), anyMap());
        verify(selfDeclarationRepository).save(any(SelfDeclaration.class));
    }

    @Test
    void testSelfDeclaration_TextCriteriaValid_ShouldSaveTextValue() {
        SelfCriteriaTextDTO textCriteria = new SelfCriteriaTextDTO("text", "desc", null, "CODE_TEXT");
        initiativeDTO.getBeneficiaryRule().setSelfDeclarationCriteria(List.of(textCriteria));

        SelfConsentTextDTO consentText = new SelfConsentTextDTO();
        consentText.setCode("CODE_TEXT");
        consentText.setValue("SomeValue");
        ConsentPutDTO consentPutDTO = new ConsentPutDTO();
        consentPutDTO.setSelfDeclarationList(List.of(consentText));

        onboardingService.selfDeclaration(initiativeDTO, consentPutDTO, "USER123");

        assertEquals("SomeValue", textCriteria.getValue());
        verify(selfDeclarationRepository).save(any(SelfDeclaration.class));
    }

    @Test
    void testSelfDeclaration_TextCriteriaNull_ShouldThrowExceptionAndAudit() {
        SelfCriteriaTextDTO textCriteria = new SelfCriteriaTextDTO("text", "desc", null, "CODE_TEXT");
        initiativeDTO.getBeneficiaryRule().setSelfDeclarationCriteria(List.of(textCriteria));

        ConsentPutDTO consentPutDTO = new ConsentPutDTO();
        consentPutDTO.setSelfDeclarationList(List.of());

        assertThrows(SelfDeclarationCrtieriaException.class,
                () -> onboardingService.selfDeclaration(initiativeDTO, consentPutDTO, "USER123"));

        verify(auditUtilities).logOnboardingKOInitiativeId(eq(initiativeDTO.getInitiativeId()),
                eq(OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_SIZE_AUDIT));
    }


    @Test
    void testSelfDeclaration_NoCriteria_ShouldReturnImmediately() {
        initiativeDTO.getBeneficiaryRule().setSelfDeclarationCriteria(List.of());
        ConsentPutDTO consentPutDTO = new ConsentPutDTO();
        consentPutDTO.setSelfDeclarationList(List.of());

        onboardingService.selfDeclaration(initiativeDTO, consentPutDTO, "USER123");

        verifyNoInteractions(auditUtilities);
        verify(selfDeclarationRepository, never()).save(any());
    }

    @Test
    void testSelfDeclaration_SizeCheckFails_ShouldThrowExceptionAndAudit() {
        initiativeDTO.getBeneficiaryRule().setSelfDeclarationCriteria(List.of(
                new SelfCriteriaBoolDTO("bool1", "desc", true, "CODE1")
        ));
        ConsentPutDTO consentPutDTO = new ConsentPutDTO();
        consentPutDTO.setSelfDeclarationList(List.of());

        doReturn(true).when(onboardingService).sizeCheck(any(), anyMap(), anyMap(), anyMap());

        assertThrows(SelfDeclarationCrtieriaException.class,
                () -> onboardingService.selfDeclaration(initiativeDTO, consentPutDTO, "USER123"));

        verify(auditUtilities).logOnboardingKOInitiativeId(eq(initiativeDTO.getInitiativeId()),
                eq(OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_SIZE_AUDIT));
    }

    @Test
    void testSelfDeclaration_TextDTOValueMissing_ShouldThrowExceptionAndAudit() {
        SelfCriteriaTextDTO textCriteria = new SelfCriteriaTextDTO("textType", "desc", null, "TEXT_CODE");
        initiativeDTO.getBeneficiaryRule().setSelfDeclarationCriteria(List.of(textCriteria));

        ConsentPutDTO consentPutDTO = new ConsentPutDTO();
        consentPutDTO.setSelfDeclarationList(List.of());

        doReturn(false).when(onboardingService).sizeCheck(any(), anyMap(), anyMap(), anyMap());

        SelfDeclarationCrtieriaException exception = assertThrows(
                SelfDeclarationCrtieriaException.class,
                () -> onboardingService.selfDeclaration(initiativeDTO, consentPutDTO, USER_ID)
        );

        verify(auditUtilities).logOnboardingKOInitiativeId(
                eq(initiativeDTO.getInitiativeId()),
                eq(OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY_AUDIT)
        );

        assertTrue(exception.getMessage().contains(initiativeDTO.getInitiativeId()));
    }


}