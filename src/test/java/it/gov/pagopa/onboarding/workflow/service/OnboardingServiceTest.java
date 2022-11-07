package it.gov.pagopa.onboarding.workflow.service;

import static com.mongodb.assertions.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import feign.FeignException;
import feign.Request;
import feign.RequestTemplate;
import it.gov.pagopa.onboarding.workflow.connector.GroupRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentMultiDTO;
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
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith({SpringExtension.class, MockitoExtension.class})
@ContextConfiguration(classes = OnboardingServiceImpl.class)
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
  GroupRestConnector groupRestConnector;

  private static final String USER_ID = "TEST_USER_ID";
  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private static final LocalDateTime OPERATION_DATE = LocalDateTime.now();

  private static final EvaluationDTO EVALUATION_DTO =
      new EvaluationDTO(
          USER_ID, INITIATIVE_ID, INITIATIVE_ID, OPERATION_DATE, INITIATIVE_ID, "ONBOARDING_OK",
          OPERATION_DATE, List.of(), new BigDecimal(500), INITIATIVE_ID);

  private static final InitiativeDTO INITIATIVE_DTO = new InitiativeDTO();
  private static final InitiativeDTO INITIATIVE_DTO_NO_PDND = new InitiativeDTO();
  private static final InitiativeDTO INITIATIVE_DTO_NO_SELF = new InitiativeDTO();
  private static final InitiativeDTO INITIATIVE_DTO_WHITELIST = new InitiativeDTO();
  private static final InitiativeDTO INITIATIVE_DTO_KO_START_DATE = new InitiativeDTO();
  private static final InitiativeDTO INITIATIVE_DTO_KO_RANKING_START_DATE = new InitiativeDTO();
  private static final InitiativeDTO INITIATIVE_DTO_KO_END_DATE = new InitiativeDTO();
  private static final InitiativeDTO INITIATIVE_DTO_KO_RANKING_END_DATE = new InitiativeDTO();
  private static final InitiativeDTO INITIATIVE_DTO_KO = new InitiativeDTO();
  private static final InitiativeBeneficiaryRuleDTO INITIATIVE_BENEFICIARY_RULE_DTO = new InitiativeBeneficiaryRuleDTO();
  private static final InitiativeBeneficiaryRuleDTO INITIATIVE_BENEFICIARY_RULE_DTO_NO_PDND = new InitiativeBeneficiaryRuleDTO();
  private static final InitiativeBeneficiaryRuleDTO INITIATIVE_BENEFICIARY_RULE_DTO_NO_SELF = new InitiativeBeneficiaryRuleDTO();
  private static final InitiativeGeneralDTO GENERAL = new InitiativeGeneralDTO();
  private static final InitiativeGeneralDTO GENERAL_WHITELIST = new InitiativeGeneralDTO();
  private static final InitiativeGeneralDTO GENERAL_KO_START_DATE = new InitiativeGeneralDTO();
  private static final InitiativeGeneralDTO GENERAL_KO_RANKING_START_DATE = new InitiativeGeneralDTO();
  private static final InitiativeGeneralDTO GENERAL_KO_END_DATE = new InitiativeGeneralDTO();
  private static final InitiativeGeneralDTO GENERAL_KO_RANKING_END_DATE = new InitiativeGeneralDTO();
  private static final InitiativeAdditionalDTO ADDITIONAL_DTO_WHITELIST = new InitiativeAdditionalDTO();
  private static final CitizenStatusDTO CITIZEN_STATUS_DTO = new CitizenStatusDTO();
  private static final CitizenStatusDTO CITIZEN_STATUS_DTO_KO = new CitizenStatusDTO();

  static {
    CITIZEN_STATUS_DTO.setStatus(true);

    CITIZEN_STATUS_DTO_KO.setStatus(false);

    GENERAL.setBeneficiaryKnown(false);
    GENERAL.setStartDate(LocalDate.MIN);
    GENERAL.setEndDate(LocalDate.MAX);

    GENERAL_WHITELIST.setBeneficiaryKnown(true);
    GENERAL_WHITELIST.setStartDate(LocalDate.MIN);
    GENERAL_WHITELIST.setEndDate(LocalDate.MAX);

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
        List.of(new SelfCriteriaBoolDTO(), new SelfCriteriaMultiDTO()));
    INITIATIVE_BENEFICIARY_RULE_DTO.setAutomatedCriteria(List.of(new AutomatedCriteriaDTO()));

    INITIATIVE_BENEFICIARY_RULE_DTO_NO_PDND.setSelfDeclarationCriteria(List.of(new SelfCriteriaBoolDTO(), new SelfCriteriaMultiDTO()));
    INITIATIVE_BENEFICIARY_RULE_DTO_NO_PDND.setAutomatedCriteria(List.of());

    INITIATIVE_BENEFICIARY_RULE_DTO_NO_SELF.setSelfDeclarationCriteria(List.of());
    INITIATIVE_BENEFICIARY_RULE_DTO_NO_SELF.setAutomatedCriteria(List.of(new AutomatedCriteriaDTO()));

    INITIATIVE_DTO.setStatus("PUBLISHED");
    INITIATIVE_DTO.setGeneral(GENERAL);
    INITIATIVE_DTO.setBeneficiaryRule(INITIATIVE_BENEFICIARY_RULE_DTO);

    INITIATIVE_DTO_NO_PDND.setStatus("PUBLISHED");
    INITIATIVE_DTO_NO_PDND.setGeneral(GENERAL);
    INITIATIVE_DTO_NO_PDND.setBeneficiaryRule(INITIATIVE_BENEFICIARY_RULE_DTO_NO_PDND);

    INITIATIVE_DTO_NO_SELF.setStatus("PUBLISHED");
    INITIATIVE_DTO_NO_SELF.setGeneral(GENERAL);
    INITIATIVE_DTO_NO_SELF.setBeneficiaryRule(INITIATIVE_BENEFICIARY_RULE_DTO_NO_SELF);

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


  @Test
  void putTc_ok_OnboardingNull() {

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.empty());

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    Mockito.doAnswer(invocationOnMock -> {
      onboarding.setTc(true);
      onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
      onboarding.setTcAcceptTimestamp(LocalDateTime.now());
      return null;
    }).when(onboardingRepositoryMock).save(Mockito.any(Onboarding.class));
    onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());

    assertEquals(INITIATIVE_ID, onboarding.getInitiativeId());
    assertEquals(USER_ID, onboarding.getUserId());
    assertEquals(OnboardingWorkflowConstants.ACCEPTED_TC, onboarding.getStatus());
    assertTrue(onboarding.getTc());
  }

  @Test
  void putTc_idemp() {

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setTc(true);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTcAcceptTimestamp(LocalDateTime.now());

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    try {
      onboardingService.putTcConsent(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      Assertions.fail();
    }

  }


  @Test
  void putTC_ko_initiative_closed() {
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.empty());
    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_KO);
    try {
      onboardingService.putTcConsent(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.FORBIDDEN.value(), e.getCode());
    }
  }

  @Test
  void putTC_ko_initiative_not_found() {
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.empty());
    Request request =
        Request.create(
            Request.HttpMethod.GET, "url", new HashMap<>(), null, new RequestTemplate());
    Mockito.doThrow(new FeignException.NotFound("", request, new byte[0], null))
        .when(initiativeRestConnector).getInitiativeBeneficiaryView(INITIATIVE_ID);
    try {
      onboardingService.putTcConsent(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.NOT_FOUND.value(), e.getCode());
    }
  }

  @Test
  void putTC_ko_unsubscribed() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.STATUS_INACTIVE);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.of(onboarding));
    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);
    try {
      onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.BAD_REQUEST.value(), e.getCode());
    }
  }

  @Test
  void getOnboardingStatus_ok() {
    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);

    OnboardingStatusDTO onboardingStatusDTO = new OnboardingStatusDTO(
        OnboardingWorkflowConstants.ACCEPTED_TC);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));
    onboardingService.getOnboardingStatus(INITIATIVE_ID, USER_ID);

    assertEquals(onboardingStatusDTO.getStatus(), onboarding.getStatus());

  }

  @Test
  void getOnboardingStatus_ko() {
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.empty());
    try {

      onboardingService.getOnboardingStatus(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {

      assertEquals(HttpStatus.NOT_FOUND.value(), e.getCode());
    }

  }

  @Test
  void checkPrerequisites_ok_no_whitelist() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    try {
      RequiredCriteriaDTO actual = onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      Assertions.fail();
    }
  }

  @Test
  void checkPrerequisites_ok_no_whitelist_no_pdnd() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_NO_PDND);

    try {
      RequiredCriteriaDTO actual = onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      Assertions.fail();
    }
  }

  @Test
  void checkPrerequisites_ok_no_whitelist_no_self() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_NO_SELF);

    try {
      RequiredCriteriaDTO actual = onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      Assertions.fail();
    }
  }

  @Test
  void checkPrerequisites_ok_whitelist() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_WHITELIST);

    Mockito.when(groupRestConnector.getCitizenStatus(INITIATIVE_ID, USER_ID))
        .thenReturn(CITIZEN_STATUS_DTO);

    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      Assertions.fail();
    }

    Mockito.verify(onboardingRepositoryMock, Mockito.times(1)).save(Mockito.any());

  }

  @Test
  void checkPrerequisites_ko_whitelist() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_WHITELIST);

    Mockito.when(groupRestConnector.getCitizenStatus(INITIATIVE_ID, USER_ID))
        .thenReturn(CITIZEN_STATUS_DTO_KO);

    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID);
      Assertions.fail();
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.FORBIDDEN.value(), e.getCode());
      assertEquals(OnboardingWorkflowConstants.ERROR_WHITELIST, e.getMessage());
    }

  }

  @Test
  void checkPrerequisites_ko_start_date() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_KO_START_DATE);

    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.FORBIDDEN.value(), e.getCode());
      assertEquals(OnboardingWorkflowConstants.ERROR_PREREQUISITES, e.getMessage());
    }
  }

  void checkPrerequisites_ko_ranking_start_date() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_KO_RANKING_START_DATE);

    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.FORBIDDEN.value(), e.getCode());
      assertEquals(OnboardingWorkflowConstants.ERROR_PREREQUISITES, e.getMessage());
    }
  }

  @Test
  void checkPrerequisites_ko_end_date() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_KO_END_DATE);

    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.FORBIDDEN.value(), e.getCode());
      assertEquals(OnboardingWorkflowConstants.ERROR_PREREQUISITES, e.getMessage());
    }
  }

  @Test
  void checkPrerequisites_ko_ranking_end_date() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_KO_RANKING_END_DATE);

    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.FORBIDDEN.value(), e.getCode());
      assertEquals(OnboardingWorkflowConstants.ERROR_PREREQUISITES, e.getMessage());
    }
  }

  @Test
  void checkPrerequisites_ko_no_tc() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.STATUS_INACTIVE);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.of(onboarding));
    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);
    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.NOT_FOUND.value(), e.getCode());
    }
  }

  @Test
  void checkPrerequisites_ko_initiative_closed() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.empty());
    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_KO);
    try {
      onboardingService.checkPrerequisites(onboarding.getInitiativeId(), onboarding.getUserId());
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.FORBIDDEN.value(), e.getCode());
    }
  }

  @Test
  void checkPrerequisites_initiative_not_found() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.empty());
    Request request =
        Request.create(
            Request.HttpMethod.GET, "url", new HashMap<>(), null, new RequestTemplate());
    Mockito.doThrow(new FeignException.NotFound("", request, new byte[0], null))
        .when(initiativeRestConnector).getInitiativeBeneficiaryView(INITIATIVE_ID);
    try {
      onboardingService.checkPrerequisites(onboarding.getInitiativeId(), onboarding.getUserId());
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.NOT_FOUND.value(), e.getCode());
    }
  }

  @Test
  void checkPrerequisites_ko_whitelist_fail() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.of(onboarding));
    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO_WHITELIST);
    Request request =
        Request.create(
            Request.HttpMethod.GET, "url", new HashMap<>(), null, new RequestTemplate());
    Mockito.doThrow(new FeignException.NotFound("", request, new byte[0], null))
        .when(groupRestConnector).getCitizenStatus(INITIATIVE_ID, USER_ID);
    try {
      onboardingService.checkPrerequisites(onboarding.getInitiativeId(), onboarding.getUserId());
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.NOT_FOUND.value(), e.getCode());
    }
  }

  @ParameterizedTest
  @CsvSource({
      "true, true, true",
      "false, false, true",
      "true, true, false",
      "true, false, false"
  })
  void saveConsent_ok(boolean pdndAccept, boolean pdndCheck, boolean autocertificationCheck) {

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", true),
        new SelfConsentMultiDTO("multi", "2", "Value"));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, pdndAccept, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setPdndCheck(pdndCheck);
    onboarding.setAutocertificationCheck(autocertificationCheck);
    onboarding.setSelfDeclarationList(
        List.of(new SelfCriteriaBoolDTO("boolean", "Descrizione booleano", true, "1"),
            new SelfCriteriaMultiDTO("multi", "Descrizione Multi", List.of("Value"), "2")));

    Mockito.when(
            onboardingRepositoryMock.findByInitiativeIdAndUserId(onboarding.getInitiativeId(), USER_ID))
        .thenReturn(
            Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
    } catch (OnboardingWorkflowException e) {
      Assertions.fail();
    }

    Mockito.verify(onboardingRepositoryMock, Mockito.times(1)).save(Mockito.any());
  }

  @Test
  void saveConsent_ko_autocertification_size() {

    List<SelfConsentDTO> selfConsentDTOList = List.of();

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, true, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setPdndCheck(true);
    onboarding.setAutocertificationCheck(true);
    onboarding.setSelfDeclarationList(
        List.of(new SelfCriteriaBoolDTO("boolean", "Descrizione booleano", true, "1"),
            new SelfCriteriaMultiDTO("multi", "Descrizione Multi", List.of("Value"), "2")));

    Mockito.when(
            onboardingRepositoryMock.findByInitiativeIdAndUserId(onboarding.getInitiativeId(), USER_ID))
        .thenReturn(
            Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.BAD_REQUEST.value(), e.getCode());
      assertEquals(OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_SIZE, e.getMessage());
    }
  }

  @Test
  void saveConsent_ko_autocertification_bool_deny() {

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", false),
        new SelfConsentMultiDTO("multi", "2", "Value"));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, true, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setPdndCheck(true);
    onboarding.setAutocertificationCheck(true);
    onboarding.setSelfDeclarationList(
        List.of(new SelfCriteriaBoolDTO("boolean", "Descrizione booleano", false, "1"),
            new SelfCriteriaMultiDTO("multi", "Descrizione Multi", List.of("Value"), "2")));

    Mockito.when(
            onboardingRepositoryMock.findByInitiativeIdAndUserId(onboarding.getInitiativeId(), USER_ID))
        .thenReturn(
            Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.BAD_REQUEST.value(), e.getCode());
      assertEquals(String.format(
          OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY,
          INITIATIVE_ID), e.getMessage());
    }
  }

  @Test
  void saveConsent_ko_autocertification_multi_invalid() {

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", true),
        new SelfConsentMultiDTO("multi", "2", "Value"));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, true, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setPdndCheck(true);
    onboarding.setAutocertificationCheck(true);
    onboarding.setSelfDeclarationList(
        List.of(new SelfCriteriaBoolDTO("boolean", "Descrizione booleano", true, "1"),
            new SelfCriteriaMultiDTO("multi", "Descrizione Multi", List.of("Value Fail"), "2")));

    Mockito.when(
            onboardingRepositoryMock.findByInitiativeIdAndUserId(onboarding.getInitiativeId(), USER_ID))
        .thenReturn(
            Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.BAD_REQUEST.value(), e.getCode());
      assertEquals(String.format(
          OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY,
          INITIATIVE_ID), e.getMessage());
    }
  }

  @Test
  void saveConsent_ko_autocertification_bool_mismatch() {

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "3", true),
        new SelfConsentMultiDTO("multi", "2", "Value"));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, true, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setPdndCheck(true);
    onboarding.setAutocertificationCheck(true);
    onboarding.setSelfDeclarationList(
        List.of(new SelfCriteriaBoolDTO("boolean", "Descrizione booleano", false, "1"),
            new SelfCriteriaMultiDTO("multi", "Descrizione Multi", List.of("Value"), "2")));

    Mockito.when(
            onboardingRepositoryMock.findByInitiativeIdAndUserId(onboarding.getInitiativeId(), USER_ID))
        .thenReturn(
            Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.BAD_REQUEST.value(), e.getCode());
      assertEquals(String.format(
          OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY,
          INITIATIVE_ID), e.getMessage());
    }
  }

  @Test
  void saveConsent_ko_autocertification_multi_mismatch() {

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", true),
        new SelfConsentMultiDTO("multi", "3", "Value"));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, true, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setPdndCheck(true);
    onboarding.setAutocertificationCheck(true);
    onboarding.setSelfDeclarationList(
        List.of(new SelfCriteriaBoolDTO("boolean", "Descrizione booleano", false, "1"),
            new SelfCriteriaMultiDTO("multi", "Descrizione Multi", List.of("Value"), "2")));

    Mockito.when(
            onboardingRepositoryMock.findByInitiativeIdAndUserId(onboarding.getInitiativeId(), USER_ID))
        .thenReturn(
            Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.BAD_REQUEST.value(), e.getCode());
      assertEquals(String.format(
          OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY,
          INITIATIVE_ID), e.getMessage());
    }
  }

  @Test
  void saveConsent_ko_pdnd() {

    List<SelfConsentDTO> selfConsentDTOList = List.of(new SelfConsentBoolDTO("boolean", "1", true),
        new SelfConsentBoolDTO("boolean", "2", true));

    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, false, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setPdndCheck(true);

    Mockito.when(
            onboardingRepositoryMock.findByInitiativeIdAndUserId(onboarding.getInitiativeId(), USER_ID))
        .thenReturn(
            Optional.of(onboarding));

    Mockito.when(initiativeRestConnector.getInitiativeBeneficiaryView(INITIATIVE_ID))
        .thenReturn(INITIATIVE_DTO);

    try {
      onboardingService.saveConsent(consentPutDTO, USER_ID);
      Assertions.fail();
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.BAD_REQUEST.value(), e.getCode());
    }
  }

  @Test
  void completeOnboarding_ok() {
    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));
    onboardingService.completeOnboarding(EVALUATION_DTO);
    assertEquals("ONBOARDING_OK", onboarding.getStatus());
  }

  @Test
  void completeOnboarding_ko() {
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.empty());
    onboardingService.completeOnboarding(EVALUATION_DTO);
    Mockito.verify(onboardingRepositoryMock, Mockito.times(1))
        .findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID);
  }

  @Test
  void deactivateOnboarding_ok() {

    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));

    Mockito.doAnswer(
            invocationOnMock -> {
              onboarding.setRequestDeactivationDate(LocalDateTime.now());
              onboarding.setStatus(OnboardingWorkflowConstants.STATUS_INACTIVE);
              return null;
            })
        .when(onboardingRepositoryMock).save(Mockito.any(Onboarding.class));
    onboardingService.deactivateOnboarding(INITIATIVE_ID, USER_ID, LocalDateTime.now().toString());
    assertNotNull(onboarding.getRequestDeactivationDate());
    assertEquals(OnboardingWorkflowConstants.STATUS_INACTIVE, onboarding.getStatus());
  }

  @Test
  void deactivateOnboarding_ko() {
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.empty());
    String date = LocalDateTime.now().toString();
    try {
      onboardingService.deactivateOnboarding(INITIATIVE_ID, USER_ID, date);
      Assertions.fail();
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.NOT_FOUND.value(), e.getCode());
    }
  }

  @Test
  void rollback() {
    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.STATUS_INACTIVE);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));
    onboardingService.rollback(INITIATIVE_ID, USER_ID);
    assertNull(onboarding.getRequestDeactivationDate());
    assertEquals(OnboardingWorkflowConstants.ONBOARDING_OK, onboarding.getStatus());
  }

  @Test
  void rollback_null() {
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.empty());
    onboardingService.rollback(INITIATIVE_ID, USER_ID);
    Mockito.verify(onboardingRepositoryMock, Mockito.times(0)).save(Mockito.any());
  }

  @Test
  void rollback_status_ko() {
    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));
    onboardingService.rollback(INITIATIVE_ID, USER_ID);
    Mockito.verify(onboardingRepositoryMock, Mockito.times(0)).save(Mockito.any());
  }

}
