package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.common.mongo.MongoTestUtilitiesService;
import it.gov.pagopa.common.web.mockvc.MockMvcUtils;
import it.gov.pagopa.onboarding.workflow.BaseIntegrationTest;
import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestClient;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestClient;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingPutDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;

class OnboardingControllerIntegrationTest extends BaseIntegrationTest {

    @Autowired
    private OnboardingRepository onboardingRepository;

    @SpyBean
    private InitiativeRestClient initiativeRestClientSpy;
    @SpyBean
    private AdmissibilityRestClient admissibilityRestClientSpy;

    private final Set<String> onboardingTestIds = new HashSet<>();

    @AfterEach
    void clearTestData(){
        onboardingRepository.deleteAllById(onboardingTestIds);
    }

//region API invokes
    protected MvcResult onboardingCitizen(String userId, String initiativeId) throws Exception {
        onboardingTestIds.add(userId);
        return mockMvc
                .perform(put("/idpay/onboarding/{userId}", userId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(new OnboardingPutDTO(initiativeId)))
                )
                .andReturn();
    }
//endregion

    @Test
    void testOnboardingCitizen() throws Exception {
        // Given
        int N=10;
        String userIdFormat = "USERID%d";
        String initiativeId = "INITIATIVE_ID";

        // When
        MongoTestUtilitiesService.startMongoCommandListener("OnboardingNewCitizen");
        for (int i = 0; i < N; i++) {
            MockMvcUtils.extractResponse(onboardingCitizen(userIdFormat.formatted(i), initiativeId), HttpStatus.NO_CONTENT, null);
        }
        MongoTestUtilitiesService.stopAndPrintMongoCommands();

        // Then
        for (int i = 0; i < N; i++) {
            Onboarding result = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userIdFormat.formatted(i)).orElse(null);
            Assertions.assertNotNull(result);
            Assertions.assertEquals(OnboardingWorkflowConstants.ACCEPTED_TC, result.getStatus());
            Assertions.assertTrue(result.getTc());
            assertTimestampValue(result.getTcAcceptTimestamp());
            assertTimestampValue(result.getUpdateDate());
        }

        Mockito.verify(initiativeRestClientSpy).getInitiativeBeneficiaryView(initiativeId);
        Mockito.verify(admissibilityRestClientSpy, Mockito.times(N)).getInitiativeStatus(initiativeId);
    }

//region assertion utils
    private static void assertTimestampValue(LocalDateTime result) {
        Assertions.assertNotNull(result);
        Assertions.assertTrue(result.isAfter(LocalDateTime.now().minusMinutes(1)));
    }
//endregion
}
