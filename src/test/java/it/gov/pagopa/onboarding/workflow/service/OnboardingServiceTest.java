package it.gov.pagopa.onboarding.workflow.service;

import static com.mongodb.assertions.Assertions.assertFalse;
import static com.mongodb.assertions.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentDTO;
import it.gov.pagopa.onboarding.workflow.dto.UnsubscribeBodyDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.dto.mapper.producer.SaveConsentDTO;
import it.gov.pagopa.onboarding.workflow.event.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
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

  @Autowired
  OnboardingService onboardingService;

  private static final String USER_ID = "TEST_USER_ID";
  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private static final String USER_ID_OK = "123";
  private static final String INITIATIVE_ID_OK = "123";
  private static final LocalDateTime OPERATION_DATE = LocalDateTime.now();

  private static final EvaluationDTO EVALUATION_DTO =
      new EvaluationDTO(
          USER_ID, INITIATIVE_ID, INITIATIVE_ID, OPERATION_DATE, INITIATIVE_ID, "ONBOARDING_OK",
          OPERATION_DATE, List.of(), new BigDecimal(500), INITIATIVE_ID);


  @Test
  void putTc_ok_OnboardingNull() {

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID_OK, USER_ID);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.empty());

    Mockito.doAnswer(invocationOnMock -> {
      onboarding.setTc(true);
      onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
      onboarding.setTcAcceptTimestamp(LocalDateTime.now());
      return null;
    }).when(onboardingRepositoryMock).save(Mockito.any(Onboarding.class));
    onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());

    assertEquals(INITIATIVE_ID_OK, onboarding.getInitiativeId());
    assertEquals(USER_ID, onboarding.getUserId());
    assertEquals(OnboardingWorkflowConstants.ACCEPTED_TC, onboarding.getStatus());
    assertTrue(onboarding.isTc());
  }

  @Test
  void putTc_idemp() {

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID_OK, USER_ID);
    onboarding.setTc(true);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTcAcceptTimestamp(LocalDateTime.now());

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID_OK, USER_ID))
        .thenReturn(
            Optional.of(onboarding));
    try {
      onboardingService.putTcConsent(INITIATIVE_ID_OK, USER_ID);
    } catch (OnboardingWorkflowException e) {
      Assertions.fail();
    }

  }


  @Test
  void putTC_ko() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.empty());
    try {
      onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.NOT_FOUND.value(), e.getCode());
    }

  }

  @Test
  void putTC_ko_inactive() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID_OK, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.STATUS_INACTIVE);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID_OK, USER_ID))
        .thenReturn(
            Optional.of(onboarding));
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
  void findByInitiativeIdAndUserId_ok() {
    try {
      Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
      Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
          .thenReturn(
              Optional.of(onboarding));
      Onboarding actual = onboardingService.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID);
      assertNotNull(actual);
      assertEquals(onboarding, actual);
    } catch (OnboardingWorkflowException e) {
      Assertions.fail();
    }
  }

  @Test
  void findByInitiativeIdAndUserId_ko() {
    try {
      Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
          .thenReturn(
              Optional.empty());
      onboardingService.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID);
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.NOT_FOUND.value(), e.getCode());
    }
  }

  @Test
  void checkTCStatus_ok() {
    try {
      Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
      onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
      onboardingService.checkTCStatus(onboarding);
    } catch (OnboardingWorkflowException e) {
      Assertions.fail();
    }
  }

  @Test
  void checkTCStatus_ko() {
    try {
      Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
      onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
      onboardingService.checkTCStatus(onboarding);
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.NOT_FOUND.value(), e.getCode());
    }
  }

  @Test
  void setOnEvaluation() {
    final Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    Mockito.doAnswer(invocationOnMock -> {
      onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
      return null;
    }).when(onboardingRepositoryMock).save(onboarding);
    onboardingService.setStatus(onboarding, OnboardingWorkflowConstants.ON_EVALUATION);
    assertEquals(OnboardingWorkflowConstants.ON_EVALUATION, onboarding.getStatus());
  }

  @Test
  void checkPrerequisites_ok() {
    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID_OK);
    } catch (OnboardingWorkflowException e) {
      Assertions.fail();
    }
  }

  @Test
  void checkPrerequisites_ko() {
    try {
      onboardingService.checkPrerequisites(INITIATIVE_ID);
      Assertions.fail();
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.FORBIDDEN.value(), e.getCode());
    }
  }

  @Test
  void checkCFWhitelist_ok() {
    boolean actual = onboardingService.checkCFWhitelist(INITIATIVE_ID, USER_ID_OK);
    assertTrue(actual);
  }

  @Test
  void checkCFWhitelist_ko() {
    boolean actual = onboardingService.checkCFWhitelist(INITIATIVE_ID, USER_ID);
    assertFalse(actual);
  }

  @Test
  void getCriteriaLists() {
    RequiredCriteriaDTO dto = onboardingService.getCriteriaLists(INITIATIVE_ID);
    assertNotNull(dto);
  }

  @Test
  void selDeclaration_Mismatch() {
    SelfConsentDTO selfConsentDTO = new SelfConsentDTO("1", true);

    List<SelfConsentDTO> selfConsentDTOList = new ArrayList<>();
    selfConsentDTOList.add(selfConsentDTO);

    ConsentPutDTO consentPutDTO = new ConsentPutDTO();

    consentPutDTO.setSelfDeclarationList(selfConsentDTOList);
    try {
      onboardingService.selfDeclaration(consentPutDTO);
      Assertions.fail();
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.BAD_REQUEST.value(), e.getCode());
    }
  }

  @Test
  void selDeclaration_Ko() {
    SelfConsentDTO selfConsentDTO = new SelfConsentDTO("1", true);
    SelfConsentDTO selfConsentDTO1 = new SelfConsentDTO("2", false);

    List<SelfConsentDTO> selfConsentDTOList = new ArrayList<>();
    selfConsentDTOList.add(selfConsentDTO);
    selfConsentDTOList.add(selfConsentDTO1);

    ConsentPutDTO consentPutDTO = new ConsentPutDTO();

    consentPutDTO.setSelfDeclarationList(selfConsentDTOList);
    try {
      onboardingService.selfDeclaration(consentPutDTO);
      Assertions.fail();
    } catch (OnboardingWorkflowException e) {
      assertEquals(HttpStatus.BAD_REQUEST.value(), e.getCode());
    }
  }

  @Test
  void selDeclaration_Ok() {

    SelfConsentDTO selfConsentDTO = new SelfConsentDTO("1", true);
    SelfConsentDTO selfConsentDTO1 = new SelfConsentDTO("2", true);

    List<SelfConsentDTO> selfConsentDTOList = new ArrayList<>();
    selfConsentDTOList.add(selfConsentDTO);
    selfConsentDTOList.add(selfConsentDTO1);

    ConsentPutDTO consentPutDTO = new ConsentPutDTO();
    consentPutDTO.setSelfDeclarationList(selfConsentDTOList);

    Map<String, Boolean> list = onboardingService.selfDeclaration(consentPutDTO);
    assertNotNull(list);

  }

  @Test
  void selDeclarationEmpty_Ok() {
    List<SelfConsentDTO> selfConsentDTOList = new ArrayList<>();

    ConsentPutDTO consentPutDTO = new ConsentPutDTO();
    consentPutDTO.setSelfDeclarationList(selfConsentDTOList);

    Map<String, Boolean> list = onboardingService.selfDeclaration(consentPutDTO);
    assertNotNull(list);

  }

  @Test
  void saveConsent_Ok() {
    SelfConsentDTO selfConsentDTO = new SelfConsentDTO("1", true);
    SelfConsentDTO selfConsentDTO1 = new SelfConsentDTO("2", true);

    List<SelfConsentDTO> selfConsentDTOList = new ArrayList<>();
    selfConsentDTOList.add(selfConsentDTO);
    selfConsentDTOList.add(selfConsentDTO1);
    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID_OK, true, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(consentPutDTO.getInitiativeId(), USER_ID);
    Mockito.when(
            onboardingRepositoryMock.findByInitiativeIdAndUserId(onboarding.getInitiativeId(), USER_ID))
        .thenReturn(
            Optional.of(onboarding));

    Mockito.doAnswer(invocationOnMock -> {
      onboarding.setSelfDeclarationList(onboardingService.selfDeclaration(consentPutDTO));
      onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
      onboarding.setPdndAccept(consentPutDTO.isPdndAccept());
      onboarding.setCriteriaConsensusTimestamp(LocalDateTime.now());
      onboardingService.checkPrerequisites(consentPutDTO.getInitiativeId());
      return null;
    }).when(onboardingRepositoryMock).save(onboarding);
    final SaveConsentDTO saveConsentDTO = new SaveConsentDTO();

    Mockito.doAnswer(invocationOnMock -> {
      saveConsentDTO.setInitiativeId(onboarding.getInitiativeId());
      saveConsentDTO.setStatus(onboarding.getStatus());
      saveConsentDTO.setCriteriaConsensusTimestamp(onboarding.getCriteriaConsensusTimestamp());
      saveConsentDTO.setPdndAccept(onboarding.getPdndAccept());
      saveConsentDTO.setSelfDeclarationList(onboarding.getSelfDeclarationList());
      return null;
    }).when(consentMapper).map(onboarding);

    Mockito.doNothing().when(onboardingProducer).sendSaveConsent(saveConsentDTO);

    onboardingService.saveConsent(consentPutDTO, onboarding.getUserId());
    assertEquals(OnboardingWorkflowConstants.ON_EVALUATION, onboarding.getStatus());
    assertNotNull(saveConsentDTO);
  }

  @Test
  void saveConsent_Ko() {
    onboardingService.getCriteriaLists(INITIATIVE_ID);
    SelfConsentDTO selfConsentDTO = new SelfConsentDTO("1", true);
    SelfConsentDTO selfConsentDTO1 = new SelfConsentDTO("2", true);

    List<SelfConsentDTO> selfConsentDTOList = new ArrayList<>();
    selfConsentDTOList.add(selfConsentDTO);
    selfConsentDTOList.add(selfConsentDTO1);
    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID_OK, false, selfConsentDTOList);

    final Onboarding onboarding = new Onboarding(consentPutDTO.getInitiativeId(), USER_ID);
    Mockito.when(
            onboardingRepositoryMock.findByInitiativeIdAndUserId(onboarding.getInitiativeId(), USER_ID))
        .thenReturn(
            Optional.of(onboarding));

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
              onboarding.setDeactivationDate(LocalDateTime.now());
              onboarding.setStatus(OnboardingWorkflowConstants.STATUS_INACTIVE);
              return null;
            })
        .when(onboardingRepositoryMock).save(Mockito.any(Onboarding.class));
    onboardingService.deactivateOnboarding(INITIATIVE_ID, USER_ID, LocalDateTime.now().toString());
    assertNotNull(onboarding.getDeactivationDate());
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
  void rollback(){
    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.STATUS_INACTIVE);
    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(Optional.of(onboarding));
    onboardingService.rollback(INITIATIVE_ID, USER_ID);
    assertNull(onboarding.getDeactivationDate());
    assertEquals(OnboardingWorkflowConstants.ONBOARDING_OK, onboarding.getStatus());
  }


}
