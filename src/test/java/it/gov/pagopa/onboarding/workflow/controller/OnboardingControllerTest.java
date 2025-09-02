package it.gov.pagopa.onboarding.workflow.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import it.gov.pagopa.common.config.JsonConfig;
import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.onboarding.workflow.config.ServiceExceptionConfig;
import it.gov.pagopa.onboarding.workflow.dto.*;
import it.gov.pagopa.onboarding.workflow.exception.custom.PageSizeNotAllowedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.UserNotOnboardedException;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ACCEPTED_TC;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.*;
import static org.mockito.Mockito.doThrow;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(value = {
    OnboardingController.class}, excludeAutoConfiguration = SecurityAutoConfiguration.class)
@Import({JsonConfig.class, ServiceExceptionConfig.class})
class OnboardingControllerTest {

  @MockitoBean
  OnboardingService onboardingService;

  @Autowired
  protected MockMvc mvc;
  private static final String BASE_URL = "http://localhost:8080/idpay/onboarding";
  private static final String DISABLE_URL = "/disable";
  private static final String ROLLBACK_URL = "/rollback";
  private static final String SUSPEND_URL = "/suspend";
  private static final String READMIT_URL = "/readmit";
  private static final String FAMILY_URL = "/family";
  private static final String USER_ID = "TEST_USER_ID";
  private static final String FAMILY_ID = "TEST_FAMILY_ID";
  private static final String CF = "TEST_CF";
  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private static final String STATUS = "STATUS";

  private static final OnboardingStatusCitizenDTO ONBOARDING_STATUS_CITIZEN_DTO = new OnboardingStatusCitizenDTO(
      USER_ID, STATUS, STATUS, null);
  static List<OnboardingStatusCitizenDTO> onboardingStatusCitizenDTOList = List.of(
      ONBOARDING_STATUS_CITIZEN_DTO);
  private static final ResponseInitiativeOnboardingDTO ONBOARDING_DTO = new ResponseInitiativeOnboardingDTO(
      onboardingStatusCitizenDTOList, 15, 20, 100, 15);
  private static final OnboardingFamilyDetailDTO ONBOARDING_FAMILY_DETAIL_DTO = new OnboardingFamilyDetailDTO(
          CF, FAMILY_ID, LocalDate.now(), STATUS);
  static List<OnboardingFamilyDetailDTO> onboardingFamilyDetailDTOList = List.of(ONBOARDING_FAMILY_DETAIL_DTO);
  private static final OnboardingFamilyDTO FAMILY_DTO = new OnboardingFamilyDTO(onboardingFamilyDetailDTOList);

    @Test
    void getOnboardingStatus_ok() throws Exception {

        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        onboarding.setStatus(ACCEPTED_TC);

        OnboardingStatusDTO onboardingStatusDTO = new OnboardingStatusDTO(
                ACCEPTED_TC, LocalDateTime.now(), null);

        Mockito.when(onboardingService.getOnboardingStatus(INITIATIVE_ID, USER_ID))
                .thenReturn(onboardingStatusDTO);

        mvc.perform(
                        MockMvcRequestBuilders.get(BASE_URL + "/" + INITIATIVE_ID + "/" + USER_ID + "/status")
                                .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON))
                .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    }

    @Test
    void getOnboardingStatus_ko() throws Exception {

        Mockito.doThrow(new UserNotOnboardedException(ID_S_NOT_FOUND_MSG, INITIATIVE_ID))
                .when(onboardingService).getOnboardingStatus(INITIATIVE_ID, USER_ID);

        mvc.perform(
                        MockMvcRequestBuilders.get(BASE_URL + "/" + INITIATIVE_ID + "/" + USER_ID + "/status")
                                .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON))
                .andExpect(MockMvcResultMatchers.status().isNotFound()).andReturn();
    }

    @Test
    void getOnboardingStatus_ko_genericServiceException() throws Exception {

        doThrow(new ServiceException("DUMMY_EXCEPTION_CODE", "DUMMY_EXCEPTION_STACK_TRACE"))
                .when(onboardingService).getOnboardingStatus(INITIATIVE_ID, USER_ID);

        mvc.perform(
                        MockMvcRequestBuilders.get(BASE_URL + "/" + INITIATIVE_ID + "/" + USER_ID + "/status")
                                .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON))
                .andExpect(MockMvcResultMatchers.status().isInternalServerError()).andReturn();
    }

    @Test
    void disableOnboarding_ok() throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        UnsubscribeBodyDTO unsubscribeBodyDTO = new UnsubscribeBodyDTO(INITIATIVE_ID, USER_ID,
                LocalDateTime.now().toString());

        Mockito.doNothing().when(onboardingService)
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
        Mockito.doNothing().when(onboardingService).rollback(INITIATIVE_ID, USER_ID);
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
                onboardingService.getOnboardingStatusList(USER_ID, null)).thenReturn(ONBOARDING_DTO);

        mvc.perform(MockMvcRequestBuilders.get(BASE_URL + "/user/" + USER_ID + "/initiative/status")
                        .contentType(MediaType.APPLICATION_JSON_VALUE)
                        .accept(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    }

    @Test
    void onboarding_status_list_ko() throws Exception {
        Mockito.doThrow(new PageSizeNotAllowedException(ERROR_MAX_NUMBER_FOR_PAGE_MSG))
                .when(onboardingService).getOnboardingStatusList(USER_ID, null);

        mvc.perform(MockMvcRequestBuilders.get(BASE_URL + "/user/" + USER_ID + "/initiative/status")
                        .contentType(MediaType.APPLICATION_JSON_VALUE)
                        .accept(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    }

    @Test
    void suspend() throws Exception {
        Mockito.doNothing().when(onboardingService).suspend(INITIATIVE_ID, USER_ID);
        mvc.perform(
                        MockMvcRequestBuilders.put(BASE_URL + "/" + INITIATIVE_ID + "/" + USER_ID + SUSPEND_URL)
                                .contentType(MediaType.APPLICATION_JSON_VALUE)
                                .accept(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(MockMvcResultMatchers.status().isNoContent())
                .andReturn();
    }
    @Test
    void readmit() throws Exception {
        Mockito.doNothing().when(onboardingService).readmit(INITIATIVE_ID, USER_ID);
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
                onboardingService.getfamilyUnitComposition(INITIATIVE_ID, USER_ID)).thenReturn(FAMILY_DTO);
        mvc.perform(
                        MockMvcRequestBuilders.get(BASE_URL + "/" + INITIATIVE_ID + "/" + USER_ID + FAMILY_URL)
                                .contentType(MediaType.APPLICATION_JSON_VALUE)
                                .accept(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andReturn();
    }

}