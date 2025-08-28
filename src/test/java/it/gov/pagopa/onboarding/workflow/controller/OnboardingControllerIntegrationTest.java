package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.common.mongo.MongoTestUtilitiesService;
import it.gov.pagopa.common.utils.TestUtils;
import it.gov.pagopa.common.web.mockvc.MockMvcUtils;
import it.gov.pagopa.onboarding.workflow.BaseIntegrationTest;
import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestClient;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestClient;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.*;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.bean.override.mockito.MockitoSpyBean;
import org.springframework.test.web.servlet.MvcResult;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;

@TestPropertySource(properties = {
        "logging.level.it.gov.pagopa.onboarding.workflow.service.OnboardingServiceImpl=WARN"
})
@Disabled
class OnboardingControllerIntegrationTest extends BaseIntegrationTest {

    public static final String INITIATIVE_ID = "INITIATIVE_ID";
    public static final String USERID_FORMAT = "USERID%d";

    @Autowired
    private OnboardingRepository onboardingRepository;

    @MockitoSpyBean
    private InitiativeRestClient initiativeRestClientSpy;
    @MockitoSpyBean
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
                        .content(objectMapper.writeValueAsString(new OnboardingSaveDTO(initiativeId)))
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

//endregion

    @Test
    void testOnboardingCitizen() {
        // Given
        int N = 10;
        int parallelism = 5;

        // When
        MongoTestUtilitiesService.startMongoCommandListener("OnboardingNewCitizen");
        TestUtils.executeParallelUseCases(N, useCases, parallelism);
        MongoTestUtilitiesService.stopAndPrintMongoCommands();

        // Then
        for (int i = 0; i < N; i++) {
            Onboarding result = onboardingRepository.findById(Onboarding.buildId(INITIATIVE_ID, USERID_FORMAT.formatted(i))).orElse(null);
            assertNotNull(result);
            assertEquals(OnboardingWorkflowConstants.ON_EVALUATION, result.getStatus());
            assertTrue(result.getTc());
            assertEquals("CHANNEL", result.getChannel());
            assertTimestampValue(result.getTcAcceptTimestamp());
            assertTimestampValue(result.getUpdateDate());
        }

        // cached client side
        Mockito.verify(initiativeRestClientSpy, Mockito.atMost(parallelism)).getInitiativeBeneficiaryView(INITIATIVE_ID);
        // executed both when accepting T&C and checkPrerequisites
        Mockito.verify(admissibilityRestClientSpy, Mockito.times(3 * N)).getInitiativeStatus(INITIATIVE_ID);

        List<ConsumerRecord<String, String>> onboardingRequestNotify = kafkaTestUtilitiesService.consumeMessages(topicOnboarding1, N, 10000);
        assertEquals(N, onboardingRequestNotify.size());
    }

//region useCases
    private final List<FailableConsumer<Integer, Exception>> useCases = List.of(

            // useCase0: successful onboarding on non-whitelist initiative
            bias -> {
                String userId = USERID_FORMAT.formatted(bias);

                // Put T&C
                MockMvcUtils.extractResponse(onboardingCitizen(userId, INITIATIVE_ID), HttpStatus.NO_CONTENT, null);

                // Get Status
                assertOnboardingStatus(
                        MockMvcUtils.extractResponse(onboardingStatus(userId, INITIATIVE_ID), HttpStatus.OK, OnboardingStatusDTO.class),
                        OnboardingWorkflowConstants.ACCEPTED_TC);

                // Get Status
                assertOnboardingStatus(
                        MockMvcUtils.extractResponse(onboardingStatus(userId, INITIATIVE_ID), HttpStatus.OK, OnboardingStatusDTO.class),
                        OnboardingWorkflowConstants.ON_EVALUATION);

            }
    );
//endregion

//region assertion utils
    private static void assertOnboardingStatus(OnboardingStatusDTO result, String expectedStatus) {
        assertNotNull(result);
        assertTimestampValue(result.getStatusDate());
        result.setStatusDate(null);

        assertEquals(
                OnboardingStatusDTO.builder()
                        .status(expectedStatus)
                        .build(),
                result);
    }

    private static void assertTimestampValue(LocalDateTime result) {
        assertNotNull(result);
        assertTrue(result.isAfter(LocalDateTime.now().minusMinutes(1)));
    }
//endregion
}
