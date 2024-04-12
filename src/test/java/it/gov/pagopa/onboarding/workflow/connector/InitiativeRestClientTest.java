package it.gov.pagopa.onboarding.workflow.connector;

import it.gov.pagopa.common.wiremock.BaseWireMockTest;
import it.gov.pagopa.onboarding.workflow.config.OnboardingWorkflowConfig;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeInvocationException;
import it.gov.pagopa.onboarding.workflow.exception.custom.InitiativeNotFoundException;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration;
import org.springframework.cloud.openfeign.FeignAutoConfiguration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;

import static it.gov.pagopa.common.wiremock.BaseWireMockTest.WIREMOCK_TEST_PROP2BASEPATH_MAP_PREFIX;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.GENERIC_ERROR;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.INITIATIVE_NOT_FOUND;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.ERROR_INITIATIVE_INVOCATION_MSG;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.INITIATIVE_NOT_FOUND_MSG;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;


@ContextConfiguration(
        classes = {
                InitiativeRestConnectorImpl.class,
                OnboardingWorkflowConfig.class,
                FeignAutoConfiguration.class,
                HttpMessageConvertersAutoConfiguration.class,
        })
@TestPropertySource(
        properties = {"spring.application.name=idpay-initiative-integration-restt",
                WIREMOCK_TEST_PROP2BASEPATH_MAP_PREFIX+"rest-client.initiative.baseUrl="})
class InitiativeRestClientTest extends BaseWireMockTest {

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
    String initiativeId = "INITIATIVE_ID_NOT_FOUND";

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
    String initiativeId = "INITIATIVE_ID_GENERIC_ERROR";

    // When
    InitiativeInvocationException exception = assertThrows(InitiativeInvocationException.class,
            () -> restConnector.getInitiativeBeneficiaryView(initiativeId));

    // Then
    assertEquals(GENERIC_ERROR,exception.getCode());
    assertEquals(ERROR_INITIATIVE_INVOCATION_MSG, exception.getMessage());
  }

}
