package it.gov.pagopa.onboarding.workflow.service;

import static com.mongodb.assertions.Assertions.assertTrue;
import static com.mongodb.assertions.Assertions.fail;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.*;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

import com.mongodb.MongoException;
import com.mongodb.MongoQueryException;
import com.mongodb.ServerAddress;
import feign.FeignException;
import feign.Request;
import feign.RequestTemplate;
import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.decrypt.DecryptRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.*;
import it.gov.pagopa.onboarding.workflow.dto.admissibility.InitiativeStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.AutomatedCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.CitizenStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeAdditionalDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeBeneficiaryRuleDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeGeneralDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.event.producer.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.event.producer.OutcomeProducer;
import it.gov.pagopa.onboarding.workflow.exception.custom.*;
import it.gov.pagopa.onboarding.workflow.exception.custom.UserNotOnboardedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.PDVInvocationException;
import it.gov.pagopa.onboarding.workflow.exception.custom.UserSuspensionOrReadmissionException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import it.gov.pagopa.onboarding.workflow.utils.AuditUtilities;
import it.gov.pagopa.onboarding.workflow.utils.Utilities;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Stream;

import org.bson.BsonDocument;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.UncategorizedMongoDbException;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith({SpringExtension.class, MockitoExtension.class})
@ContextConfiguration(classes = {OnboardingServiceImpl.class})
@TestPropertySource(
        locations = "classpath:application.yml",
        properties = {
                "app.delete.paginationSize=100",
                "app.delete.delayTime=1000"
        })
class OnboardingServiceTest {

  @MockBean
  OnboardingRepository onboardingRepositoryMock;

  @MockBean
  ConsentMapper consentMapper;

  @MockBean
  OnboardingProducer onboardingProducer;

  @MockBean
  OutcomeProducer outcomeProducer;

  @Autowired
  OnboardingService onboardingService;

  @MockBean
  InitiativeRestConnector initiativeRestConnector;

  @MockBean
  AdmissibilityRestConnector admissibilityRestConnector;

  @MockBean
  AuditUtilities auditUtilities;

  @MockBean
  Utilities utilities;

  @MockBean
  DecryptRestConnector decryptRestConnector;

  @Value("${app.delete.paginationSize}")
  private int pageSize;
  @Value("${app.delete.delayTime}")
  private long delayTime;

  private static final String USER_ID = "TEST_USER_ID";
  private static final String FAMILY_ID = "TEST_FAMILY_ID";
  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
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

  private static final BigDecimal BUDGET = BigDecimal.valueOf(1000);
  private static final BigDecimal BENEFICIARY_BUDGET = BigDecimal.valueOf(100);
  private static final String INVALID_INITIATIVE = "INVALID_INITIATIVE_ID";
  private static final String OUT_OF_RANKING = "OUT_OF_RANKING";
  private static final String INITIATIVE_REWARD_TYPE_DISCOUNT = "DISCOUNT";
  private static final String BENEFICIARY_TYPE_NF = "NF";
  private static final EvaluationDTO EVALUATION_DTO =
      new EvaluationDTO(
          USER_ID, null, INITIATIVE_ID, INITIATIVE_ID, OPERATION_DATE, INITIATIVE_ID, OnboardingWorkflowConstants.ONBOARDING_OK,
          OPERATION_DATE.atStartOfDay(), OPERATION_DATE.atStartOfDay(), List.of(),
          new BigDecimal(500), INITIATIVE_REWARD_TYPE_DISCOUNT, ORGANIZATION_NAME, false);
  private static final EvaluationDTO EVALUATION_DTO_ONBOARDING_KO_OUT_OF_RANKING =
          new EvaluationDTO(
                  USER_ID, null, INITIATIVE_ID, INITIATIVE_ID, OPERATION_DATE, INITIATIVE_ID, OnboardingWorkflowConstants.ONBOARDING_KO,
                  OPERATION_DATE.atStartOfDay(), OPERATION_DATE.atStartOfDay(),
                  List.of(new OnboardingRejectionReason(INVALID_INITIATIVE, INVALID_INITIATIVE, null, null, null),
                          new OnboardingRejectionReason(OUT_OF_RANKING, "CITIZEN_OUT_OF_RANKING", null, null, null)),
                  new BigDecimal(500), INITIATIVE_REWARD_TYPE_DISCOUNT, ORGANIZATION_NAME, false);

  private static final EvaluationDTO EVALUATION_DTO_ONBOARDING_KO =
          new EvaluationDTO(
                  USER_ID, null, INITIATIVE_ID, INITIATIVE_ID, OPERATION_DATE, INITIATIVE_ID, OnboardingWorkflowConstants.ONBOARDING_KO,
                  OPERATION_DATE.atStartOfDay(), OPERATION_DATE.atStartOfDay(),
                  List.of(new OnboardingRejectionReason(INVALID_INITIATIVE, INVALID_INITIATIVE, null, null, null)),
                  new BigDecimal(500), INITIATIVE_REWARD_TYPE_DISCOUNT, ORGANIZATION_NAME, false);

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
            new SelfCriteriaMultiDTO("multi", "", List.of("Value", "Value2"), "2")));
    INITIATIVE_BENEFICIARY_RULE_DTO.setAutomatedCriteria(List.of(AUTOMATED_CRITERIA_DTO));

    INITIATIVE_BENEFICIARY_RULE_DTO_NO_PDND.setSelfDeclarationCriteria(
        List.of(new SelfCriteriaBoolDTO("boolean", "", true, "1"),
            new SelfCriteriaMultiDTO("multi", "", List.of("Value", "Value2"), "2")));
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

  //region putTc case test

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
      assertEquals(BUDGET_EXHAUSTED, e.getCode());
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
      assertEquals(BUDGET_EXHAUSTED, e.getCode());
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
      assertEquals(BUDGET_EXHAUSTED, e.getCode());
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
    Mockito.verify(admissibilityRestConnector, Mockito.times(0)).getInitiativeStatus(any());
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
    Mockito.verify(admissibilityRestConnector, Mockito.times(0)).getInitiativeStatus(any());
  }

  @Test
  void putTc_ok_demanded_outOnboardingRange() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.DEMANDED);

    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
            .thenReturn(Optional.of(onboarding));

    LocalDate nowLocalDate = LocalDate.now();
    InitiativeDTO initiative = getInitiativeDTO(BENEFICIARY_TYPE_NF,
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
    Mockito.verify(admissibilityRestConnector, Mockito.times(0)).getInitiativeStatus(any());
  }

  @Test
  void putTc_ko_not_demanded_outOnboardingRange() {
    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
            .thenReturn(Optional.empty());

    LocalDate nowLocalDate = LocalDate.now();
    InitiativeDTO initiative = getInitiativeDTO(BENEFICIARY_TYPE_NF,
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
      Assertions.assertEquals(INITIATIVE_ENDED, e.getCode());
    }

    Mockito.verify(admissibilityRestConnector, Mockito.times(0)).getInitiativeStatus(any());
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
    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
        .thenReturn(
            Optional.empty());
    when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_KO);
    try {
      onboardingService.putTcConsent(INITIATIVE_ID, USER_ID);
    } catch (InitiativeInvalidException e) {
      assertEquals(INITIATIVE_NOT_PUBLISHED, e.getCode());
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
      assertEquals(USER_UNSUBSCRIBED, e.getCode());
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
    doThrow(new InitiativeInvalidException(INITIATIVE_ENDED, String.format(ERROR_INITIATIVE_END_MSG, INITIATIVE_ID)))
            .when(utilities).throwOnboardingKOException(anyString(), anyString());

    try {
      onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());
    } catch (InitiativeInvalidException e) {
      assertEquals(INITIATIVE_ENDED,e.getCode());
    }
  }

  //endregion

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
      assertEquals(String.format(ID_S_NOT_FOUND_MSG,INITIATIVE_ID), e.getMessage());
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

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
        .thenReturn(INITIATIVE_STATUS_DTO);

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

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
        .thenReturn(INITIATIVE_STATUS_DTO);

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
      assertEquals(INITIATIVE_ENDED, e.getCode());
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

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
            .thenReturn(INITIATIVE_STATUS_DTO);

    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL);
    } catch (InitiativeInvalidException e) {
      assertEquals(INITIATIVE_ENDED, e.getCode());
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
    when(onboardingRepositoryMock.countByInitiativeIdAndStatus(anyString(), anyString())).thenReturn(15);

    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL);
      Assertions.fail();
    } catch (InitiativeBudgetExhaustedException e) {
      Assertions.assertEquals(BUDGET_EXHAUSTED, e.getCode());
      Assertions.assertEquals(String.format(ERROR_BUDGET_TERMINATED_MSG, INITIATIVE_ID),e.getMessage());
    }
  }

  @Test
  void checkPrerequisites_ko_initiative_closed() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
        .thenReturn(
            Optional.empty());
    when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_KO);
    try {
      onboardingService.checkPrerequisites(onboarding.getInitiativeId(), onboarding.getUserId(),
          CHANNEL);
    } catch (InitiativeInvalidException e) {
      assertEquals(INITIATIVE_NOT_PUBLISHED, e.getCode());
      assertEquals(String.format(ERROR_INITIATIVE_NOT_ACTIVE_MSG, onboarding.getInitiativeId()), e.getMessage());
    }
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
    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
        .thenReturn(INITIATIVE_STATUS_DTO);
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
      assertEquals(BUDGET_EXHAUSTED, e.getCode());
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

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
        .thenReturn(INITIATIVE_STATUS_DTO);

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

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
            .thenReturn(INITIATIVE_STATUS_DTO);

    Mockito.doAnswer(invocationOnMock -> {
      onboarding.setChannel(CHANNEL);
      return null;
    }).when(onboardingRepositoryMock).save(any(Onboarding.class));

    assertDoesNotThrow(() -> onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL));

  }
  @Test
  void checkPrerequisites_ok_demanded_outOnboardingRange(){
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);
    onboarding.setDemandedDate(LocalDateTime.now());

    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
            .thenReturn(Optional.of(onboarding));

    LocalDate localDateNow = LocalDate.now();
    when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
            .thenReturn(getInitiativeDTO(BENEFICIARY_TYPE_NF,
                    localDateNow.minusDays(30),
                    localDateNow.minusDays(20),
                    localDateNow.plusDays(2),
                    localDateNow.plusDays(25)));

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
            .thenReturn(INITIATIVE_STATUS_DTO);

    Mockito.doAnswer(invocationOnMock -> {
      onboarding.setChannel(CHANNEL);
      return null;
    }).when(onboardingRepositoryMock).save(any(Onboarding.class));

    assertDoesNotThrow(() -> onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL));
  }

  @Test
  void checkPrerequisites_KO_Notdemanded_outOnboardingRange_familyInitiative(){
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
            .thenReturn(Optional.of(onboarding));

    LocalDate localDateNow = LocalDate.now();
    when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
            .thenReturn(getInitiativeDTO(BENEFICIARY_TYPE_NF,
                    localDateNow.minusDays(30),
                    localDateNow.minusDays(20),
                    localDateNow.plusDays(2),
                    localDateNow.plusDays(25)));

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
            .thenReturn(INITIATIVE_STATUS_DTO);

    Mockito.doAnswer(invocationOnMock -> {
      onboarding.setChannel(CHANNEL);
      return null;
    }).when(onboardingRepositoryMock).save(any(Onboarding.class));

    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL);
      Assertions.fail();
    } catch (InitiativeInvalidException e) {
      Assertions.assertEquals(INITIATIVE_ENDED, e.getCode());
    }
  }

  //endregion

  //region saveConsent use case
  @ParameterizedTest
  @CsvSource({
      "true, true, true",
      "false, false, true",
      "true, true, false",
      "true, false, false"
  })
  void saveConsent_ok(boolean pdndAccept, boolean pdndCheck, boolean autocertificationCheck) {

    InitiativeDTO initiativeDTO = INITIATIVE_DTO;

    if (!pdndCheck && autocertificationCheck) {
      initiativeDTO = INITIATIVE_DTO_NO_PDND;
    } else if (pdndCheck && !autocertificationCheck) {
      initiativeDTO = INITIATIVE_DTO_NO_SELF;
    } else if (!pdndCheck) {
      initiativeDTO = INITIATIVE_DTO_NO_CRITERIA;
    }

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", true),
        new SelfConsentMultiDTO("multi", "2", "Value"));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, pdndAccept, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);

    when(
            onboardingRepositoryMock.findById(Onboarding.buildId(onboarding.getInitiativeId(), USER_ID)))
        .thenReturn(
            Optional.of(onboarding));

    when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(initiativeDTO);

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
        .thenReturn(INITIATIVE_STATUS_DTO);

    assertDoesNotThrow(() -> onboardingService.saveConsent(consentPutDTO, USER_ID));

    Mockito.verify(onboardingRepositoryMock, Mockito.times(1)).save(any());
  }

  @Test
  void saveConsent_ko_autocertification_size() {

    List<SelfConsentDTO> selfConsentDTOList = List.of();

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, true, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);

    when(
            onboardingRepositoryMock.findById(Onboarding.buildId(onboarding.getInitiativeId(), USER_ID)))
        .thenReturn(
            Optional.of(onboarding));

    when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
        .thenReturn(INITIATIVE_STATUS_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (SelfDeclarationCrtieriaException e) {
      assertEquals(SELF_DECLARATION_NOT_VALID, e.getCode());
      assertEquals(String.format(ERROR_SELF_DECLARATION_NOT_VALID_MSG, consentPutDTO.getInitiativeId()), e.getMessage());
    }
  }

  @Test
  void saveConsent_ko_autocertification_bool_deny() {

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", false),
        new SelfConsentMultiDTO("multi", "2", "Value"));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, true, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);

    when(
            onboardingRepositoryMock.findById(Onboarding.buildId(onboarding.getInitiativeId(), USER_ID)))
        .thenReturn(
            Optional.of(onboarding));

    when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
        .thenReturn(INITIATIVE_STATUS_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (SelfDeclarationCrtieriaException e) {
      assertEquals(SELF_DECLARATION_NOT_VALID, e.getCode());
      assertEquals(String.format(ERROR_SELF_DECLARATION_NOT_VALID_MSG, INITIATIVE_ID), e.getMessage());
    }
  }

  @Test
  void saveConsent_ko_autocertification_multi_invalid() {

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", true),
        new SelfConsentMultiDTO("multi", "2", "Value3"));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, true, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);

    when(
            onboardingRepositoryMock.findById(Onboarding.buildId(onboarding.getInitiativeId(), USER_ID)))
        .thenReturn(
            Optional.of(onboarding));

    when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
        .thenReturn(INITIATIVE_STATUS_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (SelfDeclarationCrtieriaException e) {
      assertEquals(SELF_DECLARATION_NOT_VALID, e.getCode());
      assertEquals(String.format(ERROR_SELF_DECLARATION_NOT_VALID_MSG, INITIATIVE_ID), e.getMessage());
    }
  }

  @Test
  void saveConsent_ko_autocertification_bool_mismatch() {

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "3", true),
        new SelfConsentMultiDTO("multi", "2", "Value"));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, true, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);

    when(
            onboardingRepositoryMock.findById(Onboarding.buildId(onboarding.getInitiativeId(), USER_ID)))
        .thenReturn(
            Optional.of(onboarding));

    when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
        .thenReturn(INITIATIVE_STATUS_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (SelfDeclarationCrtieriaException e) {
      assertEquals(SELF_DECLARATION_NOT_VALID, e.getCode());
      assertEquals(String.format(ERROR_SELF_DECLARATION_NOT_VALID_MSG, INITIATIVE_ID), e.getMessage());
    }
  }

  @Test
  void saveConsent_ko_autocertification_multi_mismatch() {

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", true),
        new SelfConsentMultiDTO("multi", "3", "Value"));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, true, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);

    when(
            onboardingRepositoryMock.findById(Onboarding.buildId(onboarding.getInitiativeId(), USER_ID)))
        .thenReturn(
            Optional.of(onboarding));

    when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
        .thenReturn(INITIATIVE_STATUS_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (SelfDeclarationCrtieriaException e) {
      assertEquals(SELF_DECLARATION_NOT_VALID, e.getCode());
      assertEquals(String.format(ERROR_SELF_DECLARATION_NOT_VALID_MSG, INITIATIVE_ID), e.getMessage());
    }
  }

  @Test
  void saveConsent_ko_pdnd() {

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", true),
        new SelfConsentBoolDTO("boolean", "2", true));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, false, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);

    when(
            onboardingRepositoryMock.findById(Onboarding.buildId(onboarding.getInitiativeId(), USER_ID)))
        .thenReturn(
            Optional.of(onboarding));

    when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    when(admissibilityRestConnector.getInitiativeStatus(INITIATIVE_ID))
        .thenReturn(INITIATIVE_STATUS_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (PDNDConsentDeniedException e) {
      assertEquals(PDND_CONSENT_DENIED, e.getCode());
      assertEquals(String.format(ERROR_PDND_MSG,INITIATIVE_ID), e.getMessage());
    }
  }

  @Test
  void saveConsent_onboardingOK() {
    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", true),
            new SelfConsentBoolDTO("boolean", "2", true));
    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, false, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);

    when(onboardingRepositoryMock.findById(Onboarding.buildId(onboarding.getInitiativeId(), USER_ID)))
            .thenReturn(Optional.of(onboarding));

    assertDoesNotThrow(() ->onboardingService.saveConsent(consentPutDTO, USER_ID));
  }

  @Test
  void saveConsent_onboardingDEMANDED(){
    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", true),
            new SelfConsentBoolDTO("boolean", "2", true));
    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, false, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.DEMANDED);

    when(onboardingRepositoryMock.findById(Onboarding.buildId(onboarding.getInitiativeId(), USER_ID)))
            .thenReturn(Optional.of(onboarding));

    assertDoesNotThrow(() -> onboardingService.saveConsent(consentPutDTO, USER_ID));
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
    Mockito.verify(onboardingRepositoryMock, Mockito.times(1))
        .findById(Onboarding.buildId(INITIATIVE_ID, USER_ID));
  }
  @Test
  void completeOnboarding_ko_eligible_ko(){
    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
            .thenReturn(Optional.of(onboarding));
    onboardingService.completeOnboarding(EVALUATION_DTO_ONBOARDING_KO_OUT_OF_RANKING);
    assertEquals(OnboardingWorkflowConstants.ELIGIBLE_KO, onboarding.getStatus());
    assertEquals("CITIZEN_OUT_OF_RANKING" + ','+ INVALID_INITIATIVE, onboarding.getDetailKO());
  }

  @Test
  void completeOnboarding_ko_no_onb(){
    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
            .thenReturn(Optional.empty());
    onboardingService.completeOnboarding(EVALUATION_DTO_ONBOARDING_KO);
    Mockito.verify(onboardingRepositoryMock, Mockito.times(1))
            .findById(Onboarding.buildId(INITIATIVE_ID, USER_ID));
  }

  @Test
  void completeOnboarding_ko_no_onb_eligible_ko(){
    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
            .thenReturn(Optional.empty());
    onboardingService.completeOnboarding(EVALUATION_DTO_ONBOARDING_KO_OUT_OF_RANKING);
    Mockito.verify(onboardingRepositoryMock, Mockito.times(1))
            .findById(Onboarding.buildId(INITIATIVE_ID, USER_ID));
  }

  @Test
  void completeOnboardingCreateOnboardingStatusDEMANDED_ok() {
    EVALUATION_DTO.setStatus("DEMANDED");
    assertDoesNotThrow(() -> onboardingService.completeOnboarding(EVALUATION_DTO));
  }

  @Test
  void completeOnboarding_genericError_rejectionReason(){
    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
            .thenReturn(Optional.empty());
    EVALUATION_DTO.setOnboardingRejectionReasons(List.of(new OnboardingRejectionReason
            (INVALID_INITIATIVE, OnboardingWorkflowConstants.GENERIC_ERROR, null, null, null)));
    onboardingService.completeOnboarding(EVALUATION_DTO_ONBOARDING_KO_OUT_OF_RANKING);
    Mockito.verify(onboardingRepositoryMock, Mockito.times(1))
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
      assertEquals(String.format(ID_S_NOT_FOUND_MSG,INITIATIVE_ID), e.getMessage());
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
    Mockito.verify(onboardingRepositoryMock, Mockito.times(0)).save(any());
  }

  @Test
  void rollback_status_ko() {
    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
        .thenReturn(Optional.of(onboarding));
    onboardingService.rollback(INITIATIVE_ID, USER_ID);
    Mockito.verify(onboardingRepositoryMock, Mockito.times(0)).save(any());
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
    List<Onboarding> onboardingList = List.of(onboarding);
    Criteria criteria = new Criteria();
    Long count = 15L;
    Pageable paging = PageRequest.of(0, 20, Sort.by("lastUpdate"));

    when(
            onboardingRepositoryMock.getCriteria(INITIATIVE_ID, USER_ID, STATUS, START_DATE, END_DATE))
        .thenReturn(criteria);

    when(onboardingRepositoryMock.findByFilter(criteria, paging))
        .thenReturn(onboardingList);

    when(onboardingRepositoryMock.getCount(criteria)).thenReturn(count);

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
    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
        .thenReturn(Optional.of(onboarding));

    onboardingService.allowedInitiative(ONBOARDING_NOTIFICATION_DTO_IBAN);

    Mockito.verify(onboardingRepositoryMock, Mockito.times(0)).save(any(Onboarding.class));
  }

  @Test
  void allowedInitiative_ok_null() {
    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
        .thenReturn(Optional.empty());

    Mockito.doAnswer(
        invocationOnMock -> {
          onboarding.setStatus(anyString());
          onboarding.setInvitationDate(LocalDateTime.now());
          onboarding.setUpdateDate(LocalDateTime.now());
          onboarding.setCreationDate(LocalDateTime.now());
          return null;
        }
    ).when(onboardingRepositoryMock).save(onboarding);

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
  @ValueSource(strings = {OnboardingWorkflowConstants.SUSPENDED,OnboardingWorkflowConstants.ONBOARDING_OK})
  void readmit_ok(String status) {
    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(status);
    when(onboardingRepositoryMock.findById(Onboarding.buildId(INITIATIVE_ID, USER_ID)))
            .thenReturn(Optional.of(onboarding));

    onboardingService.readmit(INITIATIVE_ID, USER_ID);
    assertEquals(OnboardingWorkflowConstants.ONBOARDING_OK, onboarding.getStatus());
  }

  @ParameterizedTest
  @ValueSource(strings = {OnboardingWorkflowConstants.ON_EVALUATION,OnboardingWorkflowConstants.INVITED,OnboardingWorkflowConstants.ACCEPTED_TC,OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED,OnboardingWorkflowConstants.ONBOARDING_KO})
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
    Mockito.doThrow(new FeignException.InternalServerError("", request, new byte[0],headers )).when(decryptRestConnector).getPiiByToken((USER_ID));

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

    if(times == 2){
      List<Onboarding> onboardingPage = createOnboardingPage(pageSize);

      when(onboardingRepositoryMock.deletePaged(queueCommandOperationDTO.getEntityId(), pageSize))
              .thenReturn(onboardingPage)
              .thenReturn(deletedOnboardingPage);

      Thread.currentThread().interrupt();

    } else {
      when(onboardingRepositoryMock.deletePaged(queueCommandOperationDTO.getEntityId(), pageSize))
              .thenReturn(deletedOnboardingPage);
    }


    onboardingService.processCommand(queueCommandOperationDTO);


    Mockito.verify(onboardingRepositoryMock, Mockito.times(times)).deletePaged(queueCommandOperationDTO.getEntityId(), pageSize);
  }

  private static Stream<Arguments> operationTypeAndInvocationTimes() {
    return Stream.of(
            Arguments.of(OPERATION_TYPE_DELETE_INITIATIVE, 1),
            Arguments.of(OPERATION_TYPE_DELETE_INITIATIVE, 2),
            Arguments.of("OPERATION_TYPE_TEST", 0)
    );
  }

  private List<Onboarding> createOnboardingPage(int pageSize){
    List<Onboarding> onboardingPage = new ArrayList<>();

    for(int i=0;i<pageSize; i++){
      Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
      onboarding.setId("ID_ONBOARDING"+i);
      onboardingPage.add(onboarding);
    }

    return onboardingPage;
  }

  private InitiativeDTO getInitiativeDTO(String beneficiaryType,
                                         LocalDate startRankingDate,
                                         LocalDate endRankingDate,
                                         @NotNull LocalDate startDate,
                                         @NotNull LocalDate endDate) {
    InitiativeDTO initiative = new InitiativeDTO();
    InitiativeGeneralDTO general = new InitiativeGeneralDTO();

    general.setBeneficiaryKnown(false);
    if(startRankingDate != null){
      general.setRankingStartDate(startRankingDate);
    }
    if(endRankingDate != null) {
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

}
