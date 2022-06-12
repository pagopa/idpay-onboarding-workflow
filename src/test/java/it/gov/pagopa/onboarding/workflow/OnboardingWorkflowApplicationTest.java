package it.gov.pagopa.onboarding.workflow;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.controller.OnboardingController;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.service.jpa.OnboardingService;
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
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(value = {
    OnboardingController.class}, excludeAutoConfiguration = SecurityAutoConfiguration.class)
class OnboardingWorkflowApplicationTest {

  @MockBean
  OnboardingService onboardingServiceMock;

  @Autowired
  protected MockMvc mvc;

  private static final Logger LOG = LoggerFactory.getLogger(
      OnboardingWorkflowApplicationTest.class);
  private static final String BASE_URL = "http://localhost:8080/onboarding";
  private static final String USER_ID = "123";
  private static final String INITIATIVE_ID = "123";

  @Test
  void checkPrerequisitesTest_noTCAccepted() throws Exception {

    ObjectMapper objectMapper = new ObjectMapper();

    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
    onboarding.setTc(false);

    Mockito.when(onboardingServiceMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(onboarding);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    MvcResult result = mvc.perform(MockMvcRequestBuilders
            .put(BASE_URL + "/initiative/" + USER_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(objectMapper.writeValueAsString(body))
            .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isNotFound())
        .andReturn();
  }

  @Test
  void checkPrerequisitesTest_noCFInWhitelist() throws Exception {

    ObjectMapper objectMapper = new ObjectMapper();

    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingServiceMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(onboarding);
    Mockito.when(onboardingServiceMock.checkPrerequisites(INITIATIVE_ID))
        .thenReturn(true);
    Mockito.when(onboardingServiceMock.checkCFWhitelist(INITIATIVE_ID, USER_ID))
        .thenReturn(false);
    Mockito.when(onboardingServiceMock.getCriteriaLists(INITIATIVE_ID))
        .thenReturn(new RequiredCriteriaDTO(new ArrayList<>(), new ArrayList<>()));

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    // when
    MvcResult result = mvc.perform(MockMvcRequestBuilders
            .put(BASE_URL + "/initiative/" + USER_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(objectMapper.writeValueAsString(body))
            .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andReturn();

    // then
    RequiredCriteriaDTO resp = objectMapper.readValue(
        result.getResponse().getContentAsString(),
        new TypeReference<>() {
        });

    assertNotNull(resp.getPdndCriteria());
    assertNotNull(resp.getSelfDeclarationList());
  }

  @Test
  void checkPrerequisitesTest_CFInWhitelist() throws Exception {

    ObjectMapper objectMapper = new ObjectMapper();

    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingServiceMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(onboarding);
    Mockito.when(onboardingServiceMock.checkPrerequisites(INITIATIVE_ID))
        .thenReturn(true);
    Mockito.when(onboardingServiceMock.checkCFWhitelist(INITIATIVE_ID, USER_ID))
        .thenReturn(true);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    // when
    MvcResult result = mvc.perform(MockMvcRequestBuilders
            .put(BASE_URL + "/initiative/" + USER_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(objectMapper.writeValueAsString(body))
            .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isAccepted())
        .andReturn();
  }

  @Test
  void checkPrerequisitesTest_checksFailed() throws Exception {

    ObjectMapper objectMapper = new ObjectMapper();

    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
    onboarding.setTc(true);

    Mockito.when(onboardingServiceMock.findByInitiativeIdAndUserId(INITIATIVE_ID, USER_ID))
        .thenReturn(onboarding);
    Mockito.when(onboardingServiceMock.checkPrerequisites(INITIATIVE_ID))
        .thenReturn(false);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    // when
    MvcResult result = mvc.perform(MockMvcRequestBuilders
            .put(BASE_URL + "/initiative/" + USER_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(objectMapper.writeValueAsString(body))
            .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isForbidden())
        .andReturn();
  }

}