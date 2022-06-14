package it.gov.pagopa.onboarding.workflow.controller;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(value = {
    OnboardingController.class}, excludeAutoConfiguration = SecurityAutoConfiguration.class)
class OnboardingControllerTest {

  @MockBean
  OnboardingService onboardingServiceMock;

  @Autowired
  protected MockMvc mvc;

  private static final Logger LOG = LoggerFactory.getLogger(
      OnboardingControllerTest.class);
  private static final String BASE_URL = "http://localhost:8080/onboarding";
  private static final String CHECK_PREREQUISITES_URL = "/initiative/";
  private static final String USER_ID = "TEST_USER_ID";
  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";

  @Test
  void putTc_ok() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();

    Mockito.doNothing().when(onboardingServiceMock).putTcConsent(INITIATIVE_ID, USER_ID);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    MvcResult result = mvc.perform(MockMvcRequestBuilders.put(BASE_URL + "/citizen/" + USER_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(objectMapper.writeValueAsString(body)).accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isNoContent()).andReturn();
  }

  @Test
  void putTc_NotFound() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();

    Mockito.doThrow(new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
            String.format("The initiative with id %s does not exist.", INITIATIVE_ID)))
        .when(onboardingServiceMock).putTcConsent(INITIATIVE_ID, USER_ID);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    MvcResult result = mvc.perform(MockMvcRequestBuilders.put(BASE_URL + "/citizen/" + USER_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(objectMapper.writeValueAsString(body)).accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isNotFound())
        .andReturn();
  }

  @Test
  void checkPrerequisitesTest_noTCAccepted() throws Exception {

    ObjectMapper objectMapper = new ObjectMapper();

    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);

    Mockito.when(onboardingServiceMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(onboarding);

    Mockito.doThrow(new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
        String.format("Terms and Conditions have been not accepted by user %s for initiative %s.",
            USER_ID, INITIATIVE_ID))).when(onboardingServiceMock).checkTCStatus(onboarding);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    MvcResult result = mvc.perform(
            MockMvcRequestBuilders.put(BASE_URL + CHECK_PREREQUISITES_URL + USER_ID)
                .contentType(MediaType.APPLICATION_JSON_VALUE)
                .content(objectMapper.writeValueAsString(body))
                .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isNotFound()).andReturn();
  }

  @Test
  void checkPrerequisitesTest_noCFInWhitelist() throws Exception {

    ObjectMapper objectMapper = new ObjectMapper();

    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);

    Mockito.when(onboardingServiceMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(onboarding);
    Mockito.doNothing().when(onboardingServiceMock).checkPrerequisites(INITIATIVE_ID);
    Mockito.when(onboardingServiceMock.checkCFWhitelist(INITIATIVE_ID, USER_ID)).thenReturn(false);
    Mockito.when(onboardingServiceMock.getCriteriaLists(INITIATIVE_ID))
        .thenReturn(new RequiredCriteriaDTO(new ArrayList<>(), new ArrayList<>()));

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    // when
    MvcResult result = mvc.perform(
            MockMvcRequestBuilders.put(BASE_URL + CHECK_PREREQUISITES_URL + USER_ID)
                .contentType(MediaType.APPLICATION_JSON_VALUE)
                .content(objectMapper.writeValueAsString(body))
                .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();

    // then
    RequiredCriteriaDTO resp = objectMapper.readValue(result.getResponse().getContentAsString(),
        new TypeReference<>() {
        });

    assertNotNull(resp.getPdndCriteria());
    assertNotNull(resp.getSelfDeclarationList());

  }

  @Test
  void checkPrerequisitesTest_CFInWhitelist() throws Exception {

    ObjectMapper objectMapper = new ObjectMapper();

    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);

    Mockito.when(onboardingServiceMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(onboarding);
    Mockito.doNothing().when(onboardingServiceMock).checkPrerequisites(INITIATIVE_ID);
    Mockito.when(onboardingServiceMock.checkCFWhitelist(INITIATIVE_ID, USER_ID)).thenReturn(true);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    // when
    MvcResult result = mvc.perform(
            MockMvcRequestBuilders.put(BASE_URL + CHECK_PREREQUISITES_URL + USER_ID)
                .contentType(MediaType.APPLICATION_JSON_VALUE)
                .content(objectMapper.writeValueAsString(body))
                .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isAccepted()).andReturn();

  }

  @Test
  void checkPrerequisitesTest_checksFailed() throws Exception {

    ObjectMapper objectMapper = new ObjectMapper();

    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);

    Mockito.when(onboardingServiceMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(onboarding);
    Mockito.doThrow(new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
            String.format("The initiative with id %s has not met the prerequisites.", INITIATIVE_ID)))
        .when(onboardingServiceMock).checkPrerequisites(INITIATIVE_ID);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    // when
    MvcResult result = mvc.perform(
            MockMvcRequestBuilders.put(BASE_URL + CHECK_PREREQUISITES_URL + USER_ID)
                .contentType(MediaType.APPLICATION_JSON_VALUE)
                .content(objectMapper.writeValueAsString(body))
                .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isForbidden()).andReturn();

  }

  @Test
  void getOnboardingStatus_ok() throws Exception {

    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);

    OnboardingStatusDTO onboardingStatusDTO = new OnboardingStatusDTO(
        OnboardingWorkflowConstants.ACCEPTED_TC);

    Mockito.when(onboardingServiceMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(onboarding);

    Mockito.when(onboardingServiceMock.getOnboardingStatus(INITIATIVE_ID, USER_ID))
        .thenReturn(onboardingStatusDTO);

    mvc.perform(
            MockMvcRequestBuilders.get(BASE_URL + "/" + INITIATIVE_ID + "/" + USER_ID + "/status")
                .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
  }

  @Test
  void getOnboardingStatus_ko() throws Exception {

    Mockito.doThrow(new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
            String.format("Onboarding with initiativeId %s and userId %s not found.", INITIATIVE_ID,
                USER_ID)))
        .when(onboardingServiceMock).getOnboardingStatus(INITIATIVE_ID, USER_ID);

    mvc.perform(
            MockMvcRequestBuilders.get(BASE_URL + "/" + INITIATIVE_ID + "/" + USER_ID + "/status")
                .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isNotFound()).andReturn();
  }

}