package it.gov.pagopa.onboarding.workflow.controller;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.*;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
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
  private static final String BASE_URL = "http://localhost:8080/idpay/onboarding";
  private static final String DISABLE_URL = "/disable";
  private static final String ROLLBACK_URL = "/rollback";
  private static final String SUSPEND_URL = "/suspend";
  private static final String READMIT_URL = "/readmit";
  private static final String FAMILY_URL = "/family";
  private static final String CHECK_PREREQUISITES_URL = "/initiative/";
  private static final String USER_ID = "TEST_USER_ID";
  private static final String FAMILY_ID = "TEST_FAMILY_ID";
  private static final String CF = "TEST_CF";
  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private static final LocalDateTime START_DATE = LocalDateTime.now();
  private static final LocalDateTime END_DATE = LocalDateTime.now();
  private static final String STATUS = "STATUS";
  private static final String CHANNEL = "CHANNEL";

  private static final OnboardingStatusCitizenDTO ONBOARDING_STATUS_CITIZEN_DTO = new OnboardingStatusCitizenDTO(
      USER_ID, STATUS, STATUS);
  static List<OnboardingStatusCitizenDTO> onboardingStatusCitizenDTOList = List.of(
      ONBOARDING_STATUS_CITIZEN_DTO);
  private static final ResponseInitiativeOnboardingDTO ONBOARDING_DTO = new ResponseInitiativeOnboardingDTO(
      onboardingStatusCitizenDTOList, 15, 20, 100, 15);
  private static final OnboardingFamilyDetailDTO ONBOARDING_FAMILY_DETAIL_DTO = new OnboardingFamilyDetailDTO(
          CF, FAMILY_ID, LocalDate.now(), STATUS);
  static List<OnboardingFamilyDetailDTO> onboardingFamilyDetailDTOList = List.of(ONBOARDING_FAMILY_DETAIL_DTO);
  private static final OnboardingFamilyDTO FAMILY_DTO = new OnboardingFamilyDTO(onboardingFamilyDetailDTOList);

  @Test
  void putTc_ok() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();

    Mockito.doNothing().when(onboardingServiceMock).putTcConsent(INITIATIVE_ID, USER_ID);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    mvc.perform(MockMvcRequestBuilders.put(BASE_URL + "/" + USER_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(objectMapper.writeValueAsString(body))
            .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isNoContent()).andReturn();

  }

  @Test
  void putTc_NotFound() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();

    Mockito.doThrow(new OnboardingWorkflowException(HttpStatus.NOT_FOUND.value(),
            String.format("The initiative with id %s does not exist.", INITIATIVE_ID), OnboardingWorkflowConstants.GENERIC_ERROR))
        .when(onboardingServiceMock).putTcConsent(INITIATIVE_ID, USER_ID);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);

    mvc.perform(MockMvcRequestBuilders.put(BASE_URL + "/" + USER_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(objectMapper.writeValueAsString(body)).accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isNotFound())
        .andReturn();
  }

  @Test
  void putTc_blankBody() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", "");

    mvc.perform(MockMvcRequestBuilders.put(BASE_URL + "/" + USER_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(objectMapper.writeValueAsString(body)).accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isBadRequest()).andReturn();
  }

  @Test
  void checkPrerequisitesTest_blankBody() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", "");
    body.put("channel", CHANNEL);

    mvc.perform(
            MockMvcRequestBuilders.put(BASE_URL + CHECK_PREREQUISITES_URL + USER_ID)
                .contentType(MediaType.APPLICATION_JSON_VALUE)
                .content(objectMapper.writeValueAsString(body))
                .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isBadRequest()).andReturn();
  }


  @Test
  void checkPrerequisitesTest_noCFInWhitelist() throws Exception {

    ObjectMapper objectMapper = new ObjectMapper();

    Mockito.when(onboardingServiceMock.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL))
        .thenReturn(new RequiredCriteriaDTO(new ArrayList<>(), new ArrayList<>()));

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);
    body.put("channel", CHANNEL);

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

    Mockito.when(onboardingServiceMock.checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL)).thenReturn(null);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);
    body.put("channel", CHANNEL);

    // when
    mvc.perform(
            MockMvcRequestBuilders.put(BASE_URL + CHECK_PREREQUISITES_URL + USER_ID)
                .contentType(MediaType.APPLICATION_JSON_VALUE)
                .content(objectMapper.writeValueAsString(body))
                .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isAccepted()).andReturn();

  }

  @Test
  void checkPrerequisitesTest_checksFailed() throws Exception {

    ObjectMapper objectMapper = new ObjectMapper();

    Mockito.doThrow(new OnboardingWorkflowException(HttpStatus.FORBIDDEN.value(),
                    OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED_MSG,
                    OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED))
        .when(onboardingServiceMock).checkPrerequisites(INITIATIVE_ID, USER_ID, CHANNEL);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", INITIATIVE_ID);
    body.put("channel", CHANNEL);

    // when
    mvc.perform(
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
            String.format("Onboarding with initiativeId %s and current userId not found.",
                INITIATIVE_ID), OnboardingWorkflowConstants.GENERIC_ERROR))
        .when(onboardingServiceMock).getOnboardingStatus(INITIATIVE_ID, USER_ID);

    mvc.perform(
            MockMvcRequestBuilders.get(BASE_URL + "/" + INITIATIVE_ID + "/" + USER_ID + "/status")
                .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isNotFound()).andReturn();
  }

  @Test
  void saveConsent_ok() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    SelfConsentDTO selfConsentDTO = new SelfConsentBoolDTO("boolean", "1", true);
    SelfConsentDTO selfConsentDTO1 = new SelfConsentBoolDTO("boolean", "2", true);

    List<SelfConsentDTO> selfConsentDTOList = new ArrayList<>();
    selfConsentDTOList.add(selfConsentDTO);
    selfConsentDTOList.add(selfConsentDTO1);
    ConsentPutDTO consentPutDTO = new ConsentPutDTO(INITIATIVE_ID, true, selfConsentDTOList);

    Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
    onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);

    Mockito.doNothing().when(onboardingServiceMock).saveConsent(consentPutDTO, USER_ID);

    Map<String, Object> body = new HashMap<>();
    body.put("initiativeId", consentPutDTO.getInitiativeId());
    body.put("pdndAccept", consentPutDTO.isPdndAccept());
    body.put("selfDeclarationList", consentPutDTO.getSelfDeclarationList());

    mvc.perform(MockMvcRequestBuilders.put(BASE_URL + "/consent/" + USER_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(objectMapper.writeValueAsString(body))
            .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isAccepted()).andReturn();

  }

  @Test
  void disableOnboarding_ok() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    UnsubscribeBodyDTO unsubscribeBodyDTO = new UnsubscribeBodyDTO(INITIATIVE_ID, USER_ID,
        LocalDateTime.now().toString());

    Mockito.doNothing().when(onboardingServiceMock)
        .deactivateOnboarding(INITIATIVE_ID, USER_ID, LocalDateTime.now().toString());

    mvc.perform(
            MockMvcRequestBuilders.delete(BASE_URL + DISABLE_URL)
                .contentType(MediaType.APPLICATION_JSON_VALUE)
                .content(objectMapper.writeValueAsString(unsubscribeBodyDTO))
                .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isNoContent())
        .andReturn();
  }

  @Test
  void rollback() throws Exception {
    Mockito.doNothing().when(onboardingServiceMock).rollback(INITIATIVE_ID, USER_ID);
    mvc.perform(
            MockMvcRequestBuilders.put(BASE_URL + ROLLBACK_URL + "/" + INITIATIVE_ID + "/" + USER_ID)
                .contentType(MediaType.APPLICATION_JSON_VALUE)
                .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isNoContent())
        .andReturn();
  }

  @Test
  void onboarding_status_list_ok() throws Exception {
    Mockito.when(
        onboardingServiceMock.getOnboardingStatusList(INITIATIVE_ID, USER_ID, START_DATE, END_DATE,
            STATUS, null)).thenReturn(ONBOARDING_DTO);

    mvc.perform(MockMvcRequestBuilders.get(BASE_URL + "/" + INITIATIVE_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
  }

  @Test
  void onboarding_status_list_ko() throws Exception {
    Mockito.doThrow(new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(),
        OnboardingWorkflowConstants.ERROR_MAX_NUMBER_FOR_PAGE, OnboardingWorkflowConstants.GENERIC_ERROR)).when(
        onboardingServiceMock).getOnboardingStatusList(INITIATIVE_ID, USER_ID, START_DATE, END_DATE,
        STATUS, null);

    mvc.perform(MockMvcRequestBuilders.get(BASE_URL + "/" + INITIATIVE_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
  }

  @Test
  void suspend() throws Exception {
    Mockito.doNothing().when(onboardingServiceMock).suspend(INITIATIVE_ID, USER_ID);
    mvc.perform(
                    MockMvcRequestBuilders.put(BASE_URL + "/" + INITIATIVE_ID + "/" + USER_ID + SUSPEND_URL)
                            .contentType(MediaType.APPLICATION_JSON_VALUE)
                            .accept(MediaType.APPLICATION_JSON_VALUE))
            .andExpect(MockMvcResultMatchers.status().isNoContent())
            .andReturn();
  }
  @Test
  void readmit() throws Exception {
    Mockito.doNothing().when(onboardingServiceMock).suspend(INITIATIVE_ID, USER_ID);
    mvc.perform(
                    MockMvcRequestBuilders.put(BASE_URL + "/" + INITIATIVE_ID + "/" + USER_ID + READMIT_URL)
                            .contentType(MediaType.APPLICATION_JSON_VALUE)
                            .accept(MediaType.APPLICATION_JSON_VALUE))
            .andExpect(MockMvcResultMatchers.status().isNoContent())
            .andReturn();
  }

  @Test
  void familyUnitComposition() throws Exception {
    Mockito.when(
            onboardingServiceMock.getfamilyUnitComposition(INITIATIVE_ID, USER_ID)).thenReturn(FAMILY_DTO);
    mvc.perform(
            MockMvcRequestBuilders.get(BASE_URL + "/" + INITIATIVE_ID + "/" + USER_ID + FAMILY_URL)
                            .contentType(MediaType.APPLICATION_JSON_VALUE)
                            .accept(MediaType.APPLICATION_JSON_VALUE))
            .andExpect(MockMvcResultMatchers.status().isOk())
            .andReturn();
  }
}