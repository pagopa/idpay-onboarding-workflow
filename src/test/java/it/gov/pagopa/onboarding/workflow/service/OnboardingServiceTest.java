package it.gov.pagopa.onboarding.workflow.service;

import static com.mongodb.assertions.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import java.time.Instant;
import java.util.Date;
import java.util.Optional;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(value = {
    OnboardingService.class})
class OnboardingServiceTest {

  @MockBean
  OnboardingRepository onboardingRepositoryMock;

  @Autowired
  OnboardingService onboardingService;

  private static final String USER_ID = "TEST_USER_ID";
  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private static final String USER_ID_OK = "123";
  private static final String INITIATIVE_ID_OK = "123";

  @Test
  void putTc_ok(){

    final Onboarding onboarding = new Onboarding(INITIATIVE_ID_OK, USER_ID);

    Mockito.when(onboardingRepositoryMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(
            Optional.empty());

    Mockito.doAnswer(invocationOnMock -> {
      onboarding.setTc(true);
      onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
      onboarding.setTcAcceptTimestamp(Date.from(Instant.now()));
      return null;
    }).when(onboardingRepositoryMock).save(Mockito.any(Onboarding.class));
    onboardingService.putTcConsent(onboarding.getInitiativeId(), onboarding.getUserId());

    assertEquals(INITIATIVE_ID_OK, onboarding.getInitiativeId());
    assertEquals(USER_ID, onboarding.getUserId());
    assertEquals(OnboardingWorkflowConstants.ACCEPTED_TC, onboarding.getStatus());
    assertTrue(onboarding.isTc());
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
    } catch(OnboardingWorkflowException e){
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
    } catch(OnboardingWorkflowException e){
      assertEquals(HttpStatus.NOT_FOUND.value(), e.getCode());
    }
  }

  @Test
  void checkTCStatus_ok() {
    try {
      Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
      onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
      onboarding.setTc(true);
      onboardingService.checkTCStatus(onboarding);
    } catch(OnboardingWorkflowException e){
      Assertions.fail();
    }
  }

  @Test
  void checkTCStatus_ko() {
    try {
      Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
      onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
      onboarding.setTc(false);
      onboardingService.checkTCStatus(onboarding);
    } catch(OnboardingWorkflowException e){
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
    onboardingService.setOnEvaluation(onboarding);
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
  void checkCFWhitelist() {
    boolean actual = onboardingService.checkCFWhitelist(INITIATIVE_ID_OK, USER_ID_OK);
    assertTrue(actual);
  }

  @Test
  void getCriteriaLists() {
    RequiredCriteriaDTO dto = onboardingService.getCriteriaLists(INITIATIVE_ID);
    assertNotNull(dto);
  }

}
