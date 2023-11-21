package it.gov.pagopa.onboarding.workflow.connector;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import it.gov.pagopa.onboarding.workflow.config.OnboardingWorkflowConfig;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestClient;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestConnectorImpl;
import it.gov.pagopa.onboarding.workflow.dto.admissibility.InitiativeStatusDTO;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import it.gov.pagopa.onboarding.workflow.exception.custom.AdmissibilityInvocationException;
import it.gov.pagopa.onboarding.workflow.service.OnboardingService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cloud.openfeign.FeignAutoConfiguration;
import org.springframework.context.ApplicationContextInitializer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.support.TestPropertySourceUtils;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.GENERIC_ERROR;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.INITIATIVE_NOT_FOUND;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.ERROR_ADMISSIBILITY_INVOCATION_MSG;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.INITIATIVE_NOT_FOUND_MSG;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@ContextConfiguration(
    initializers = AdmissibilityRestClientTest.WireMockInitializer.class,
    classes = {
        AdmissibilityRestConnectorImpl.class,
        OnboardingWorkflowConfig.class,
        FeignAutoConfiguration.class,
        HttpMessageConvertersAutoConfiguration.class
    })
@TestPropertySource(
    locations = "classpath:application.yml",
    properties = {"spring.application.name=idpay-admissibility-integration-rest"})
//@AutoConfigureWireMock(stubs = "classpath:/mappings")
class AdmissibilityRestClientTest {

  @Autowired
  private AdmissibilityRestClient restClient;

  @Autowired
  private AdmissibilityRestConnector restConnector;

  @MockBean
  OnboardingService onboardingService;

  @Test
  void getInitiativeStatus(){
    // Given
    String initiativeId = "INITIATIVE_ID";

    // When
    InitiativeStatusDTO actual = restConnector.getInitiativeStatus(initiativeId);

    // Then
    assertTrue(actual.isBudgetAvailable());
  }

  @Test
  void getInitiativeStatus_NOT_FOUND(){
    // Given
    String initiativeId = "INITIATIVE_ID_NOT_FOUND";

    // When
    InitiativeNotFoundException exception = assertThrows(InitiativeNotFoundException.class,
            () -> restConnector.getInitiativeStatus(initiativeId));

    // Then
    assertEquals(INITIATIVE_NOT_FOUND,exception.getCode());
    assertEquals(String.format(INITIATIVE_NOT_FOUND_MSG, initiativeId), exception.getMessage());
  }

  @Test
  void getInitiativeStatus_GENERIC_ERROR(){
    // Given
    String initiativeId = "INITIATIVE_ID_GENERIC_ERROR";

    // When
    AdmissibilityInvocationException exception = assertThrows(AdmissibilityInvocationException.class,
            () -> restConnector.getInitiativeStatus(initiativeId));

    // Then
    assertEquals(GENERIC_ERROR,exception.getCode());
    assertEquals(ERROR_ADMISSIBILITY_INVOCATION_MSG, exception.getMessage());
  }


  public static class WireMockInitializer
      implements ApplicationContextInitializer<ConfigurableApplicationContext> {

    @Override
    public void initialize(ConfigurableApplicationContext applicationContext) {
      WireMockServer wireMockServer = new WireMockServer(new WireMockConfiguration().dynamicPort());
      wireMockServer.start();

      applicationContext.getBeanFactory().registerSingleton("wireMockServer", wireMockServer);

      applicationContext.addApplicationListener(
          applicationEvent -> {
            if (applicationEvent instanceof ContextClosedEvent) {
              wireMockServer.stop();
            }
          });

      TestPropertySourceUtils.addInlinedPropertiesToEnvironment(
          applicationContext,
          String.format(
              "rest-client.admissibility.baseUrl=http://%s:%d",
              wireMockServer.getOptions().bindAddress(), wireMockServer.port()));
    }
  }

}
