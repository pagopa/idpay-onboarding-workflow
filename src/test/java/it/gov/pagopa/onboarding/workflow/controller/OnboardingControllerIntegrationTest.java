package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.common.mongo.MongoTestUtilitiesService;
import it.gov.pagopa.common.web.mockvc.MockMvcUtils;
import it.gov.pagopa.onboarding.workflow.BaseIntegrationTest;
import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestClient;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestClient;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.CheckPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
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

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;

class OnboardingControllerIntegrationTest extends BaseIntegrationTest {

    public static final String INITIATIVE_ID = "INITIATIVE_ID";
    public static final String USERID_FORMAT = "USERID%d";

    @Autowired
    private OnboardingRepository onboardingRepository;

    @SpyBean
    private InitiativeRestClient initiativeRestClientSpy;
    @SpyBean
    private AdmissibilityRestClient admissibilityRestClientSpy;

    private final Set<String> onboardingTestIds = new HashSet<>();

    @AfterEach
    void clearTestData() {
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

    protected MvcResult onboardingStatus(String userId, String initiativeId) throws Exception {
        onboardingTestIds.add(userId);
        return mockMvc
                .perform(get("/idpay/onboarding/{initiativeId}/{userId}/status", initiativeId, userId)
                        .contentType(MediaType.APPLICATION_JSON)
                )
                .andReturn();
    }

    protected MvcResult checkPrerequisites(String userId, String initiativeId) throws Exception {
        onboardingTestIds.add(userId);
        return mockMvc
                .perform(put("/idpay/onboarding/initiative/{userId}", userId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(new CheckPutDTO(initiativeId, "CHANNEL")))
                )
                .andReturn();
    }
//endregion

    @Test
    void testOnboardingCitizen() throws Exception {
        // Given
        int N = 10;

        // When
        MongoTestUtilitiesService.startMongoCommandListener("OnboardingNewCitizen");
        for (int i = 0; i < N; i++) {
            onboardCitizen(i);
        }
        MongoTestUtilitiesService.stopAndPrintMongoCommands();

        // Then
        for (int i = 0; i < N; i++) {
            Onboarding result = onboardingRepository.findById(Onboarding.buildId(INITIATIVE_ID, USERID_FORMAT.formatted(i))).orElse(null);
            Assertions.assertNotNull(result);
            Assertions.assertEquals(OnboardingWorkflowConstants.ACCEPTED_TC, result.getStatus());
            Assertions.assertTrue(result.getTc());
            Assertions.assertEquals("CHANNEL", result.getChannel());
            assertTimestampValue(result.getTcAcceptTimestamp());
            assertTimestampValue(result.getUpdateDate());
        }

        // cached client side
        Mockito.verify(initiativeRestClientSpy).getInitiativeBeneficiaryView(INITIATIVE_ID);
        // executed both when accepting T&C and checkPrerequisites
        Mockito.verify(admissibilityRestClientSpy, Mockito.times(2*N)).getInitiativeStatus(INITIATIVE_ID);
    }

//region useCases

    // useCase successful onboarding on non-whitelist initiative
    private void onboardCitizen(int bias) throws Exception {
        String userId = USERID_FORMAT.formatted(bias);

        // Put T&C
        MockMvcUtils.extractResponse(onboardingCitizen(userId, INITIATIVE_ID), HttpStatus.NO_CONTENT, null);

        // Get Status
        assertOnboardingStatus(
                MockMvcUtils.extractResponse(onboardingStatus(userId, INITIATIVE_ID), HttpStatus.OK, OnboardingStatusDTO.class),
                OnboardingWorkflowConstants.ACCEPTED_TC);

        // Put AcceptPrerequisites
        MockMvcUtils.extractResponse(checkPrerequisites(userId, INITIATIVE_ID), HttpStatus.OK, RequiredCriteriaDTO.class);

    }
//endregion

//region assertion utils
    private static void assertOnboardingStatus(OnboardingStatusDTO result, String expectedStatus) {
        Assertions.assertNotNull(result);
        assertTimestampValue(result.getStatusDate());
        result.setStatusDate(null);

        Assertions.assertEquals(
                OnboardingStatusDTO.builder()
                        .status(expectedStatus)
                        .build(),
                result);
    }

    private static void assertTimestampValue(LocalDateTime result) {
        Assertions.assertNotNull(result);
        Assertions.assertTrue(result.isAfter(LocalDateTime.now().minusMinutes(1)));
    }
//endregion
}
