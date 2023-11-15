package it.gov.pagopa.onboarding.workflow.connector;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.GENERIC_ERROR;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.INITIATIVE_NOT_FOUND;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.ERROR_INITIATIVE_INVOCATION_MSG;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.INITIATIVE_NOT_FOUND_MSG;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import it.gov.pagopa.onboarding.workflow.config.OnboardingWorkflowConfig;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.exception.custom.notfound.InitiativeNotFoundException;
import it.gov.pagopa.onboarding.workflow.exception.custom.servererror.InitiativeInvocationException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.cloud.openfeign.FeignAutoConfiguration;
import org.springframework.context.ApplicationContextInitializer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.support.TestPropertySourceUtils;

@ExtendWith(SpringExtension.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@ContextConfiguration(
    initializers = InitiativeRestClientTest.WireMockInitializer.class,
    classes = {
        InitiativeRestConnectorImpl.class,
        OnboardingWorkflowConfig.class,
        FeignAutoConfiguration.class,
        HttpMessageConvertersAutoConfiguration.class
    })
@TestPropertySource(
    locations = "classpath:application.yml",
    properties = {"spring.application.name=idpay-initiative-integration-rest"})
class InitiativeRestClientTest {

  private static final String INITIATIVE_ID = "INITIATIVE_ID";

  @Autowired
  private InitiativeRestClient restClient;

  @Autowired
  private InitiativeRestConnector restConnector;

  @Test
  void getInitiativeBeneficiaryView() {

    InitiativeDTO actual = restConnector.getInitiativeBeneficiaryView(INITIATIVE_ID);

    assertEquals(INITIATIVE_ID, actual.getInitiativeId());
  }

  @Test
  void getInitiativeStatus_NOT_FOUND(){
    // Given
    String initiativeId = "INITIATIVEID_NOT_FOUND";

    // When
    InitiativeNotFoundException exception = assertThrows(InitiativeNotFoundException.class,
            () -> restConnector.getInitiativeBeneficiaryView(initiativeId));

    // Then
    assertEquals(INITIATIVE_NOT_FOUND,exception.getCode());
    assertEquals(String.format(INITIATIVE_NOT_FOUND_MSG, initiativeId), exception.getMessage());
  }

  @Test
  void getInitiativeStatus_GENERIC_ERROR(){
    // Given
    String initiativeId = "INITIATIVEID_GENERIC_ERROR";

    // When
    InitiativeInvocationException exception = assertThrows(InitiativeInvocationException.class,
            () -> restConnector.getInitiativeBeneficiaryView(initiativeId));

    // Then
    assertEquals(GENERIC_ERROR,exception.getCode());
    assertEquals(ERROR_INITIATIVE_INVOCATION_MSG, exception.getMessage());
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
              "rest-client.initiative.baseUrl=http://%s:%d",
              wireMockServer.getOptions().bindAddress(), wireMockServer.port()));
    }
  }
}
