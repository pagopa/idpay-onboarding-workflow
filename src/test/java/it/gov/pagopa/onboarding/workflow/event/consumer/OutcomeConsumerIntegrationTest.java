package it.gov.pagopa.onboarding.workflow.event.consumer;

import it.gov.pagopa.common.mongo.retry.MongoRequestRateTooLargeRetryableTest;
import it.gov.pagopa.common.utils.TestUtils;
import it.gov.pagopa.onboarding.workflow.BaseIntegrationTest;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingRejectionReason;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.IntStream;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.kafka.clients.consumer.OffsetAndMetadata;
import org.apache.kafka.common.TopicPartition;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.SpyBean;

class OutcomeConsumerIntegrationTest extends BaseIntegrationTest {

    @SpyBean
    private OnboardingRepository onboardingRepositorySpy;

    @AfterEach
    void clearData() {
        onboardingRepositorySpy.deleteAll();
    }

    @Test
    void test() {
        // Configuring test useCases
        int N = 10;

        List<EvaluationDTO> payloads = new ArrayList<>(IntStream.range(0, N).mapToObj(this::buildValidPayload).toList());
        List<EvaluationDTO> skippedEntities = buildSkippedEntities();
        payloads.addAll(skippedEntities);

        // Sending messages
        int totalSendMessages = payloads.size();
        int expectedStoredOnboarding = totalSendMessages - skippedEntities.size();

        long timePublishOnboardingStart = System.currentTimeMillis();
        payloads.forEach(p -> kafkaTestUtilitiesService.publishIntoEmbeddedKafka(topicConsumerOutcome, null, null, p));
        long timePublishingOnboardingEnd = System.currentTimeMillis();

        // Waiting end of processing
        Assertions.assertEquals(
                expectedStoredOnboarding,
                waitForRewardsStored(expectedStoredOnboarding));

        // Checking results
        onboardingRepositorySpy.findAll().forEach(this::checkOnboardingResulted);

        // Print results
        long timeEnd = System.currentTimeMillis();

        System.out.printf("""
                        ************************
                        Time spent to send %d trx messages: %d millis
                        Time spent to consume reward responses: %d millis
                        ************************
                        Test Completed in %d millis
                        ************************
                        """,
                totalSendMessages,
                timePublishingOnboardingEnd - timePublishOnboardingStart,
                timeEnd - timePublishingOnboardingEnd,
                timeEnd - timePublishOnboardingStart
        );

        checkOffsets(totalSendMessages);
    }

    // region test utility methods
    private List<EvaluationDTO> buildSkippedEntities() {
        // Skipped because not related to a stored Onboarding
        EvaluationDTO skipped = new EvaluationDTO();
        skipped.setUserId("SKIPPEDUSERID0");
        skipped.setInitiativeId("INITIATIVEID");
        skipped.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
        return List.of(skipped);
    }

    private EvaluationDTO buildValidPayload(int bias) {
        return useCases.get(bias % useCases.size()).getLeft().apply(bias);
    }

    protected long waitForRewardsStored(int n) {
        long[] countSaved = {0};
        TestUtils.waitFor(() -> (countSaved[0] = onboardingRepositorySpy.findAll().size()) >= n, () -> "Expected %d saved onboarding requests, read %d".formatted(n, countSaved[0]), 60, 1000);
        return countSaved[0];
    }

    private void checkOnboardingResulted(Onboarding onboarding) {
        String userId = onboarding.getUserId();
        int biasRetrieve = Integer.parseInt(userId.substring(6));
        useCases.get(biasRetrieve % useCases.size()).getValue().accept(onboarding);
    }

    protected void checkOffsets(long expectedReadMessages) {
        long timeStart = System.currentTimeMillis();
        final Map<TopicPartition, OffsetAndMetadata> srcCommitOffsets = kafkaTestUtilitiesService.checkCommittedOffsets(topicConsumerOutcome, groupIdConsumerOutcome, expectedReadMessages, 20, 1000);
        long timeCommitChecked = System.currentTimeMillis();

        System.out.printf("""
                        ************************
                        Time occurred to check committed offset: %d millis
                        ************************
                        Source Topic Committed Offsets: %s
                        ************************
                        """,
                timeCommitChecked - timeStart,
                srcCommitOffsets
        );
    }
    // endregion

    //region useCases utility methods
    private Onboarding storeOnboardingRequest(String initiativeId, String userId, String status) {
        Onboarding onboarding = new Onboarding(initiativeId, userId);
        onboarding.setStatus(status);
        onboarding.setCreationDate(LocalDateTime.now());
        return onboardingRepositorySpy.save(onboarding);
    }
    //endregion

    //region useCases
    private final List<Pair<Function<Integer, EvaluationDTO>, Consumer<Onboarding>>> useCases = List.of(
            // useCase0: receiving ONBOARDING_OK on existent onboarding request having status ACCEPTED_TC
            Pair.of(
                    i -> {
                        String initiativeId = "INITIATIVEID" + i;
                        String userId = "USERID" + i;
                        EvaluationDTO out = new EvaluationDTO();
                        out.setUserId(userId);
                        out.setInitiativeId(initiativeId);
                        out.setStatus(OnboardingWorkflowConstants.ONBOARDING_OK);
                        out.setAdmissibilityCheckDate(LocalDateTime.now());
                        out.setFamilyId("FAMILYID");

                        storeOnboardingRequest(initiativeId, userId, OnboardingWorkflowConstants.ACCEPTED_TC);

                        return out;
                    },
                    o -> {
                        assertOnboardingUpdated(o, OnboardingWorkflowConstants.ONBOARDING_OK);
                        Assertions.assertEquals("FAMILYID", o.getFamilyId());
                    }
            ),

            // useCase1: receiving ONBOARDING_KO on existent onboarding request having status ACCEPTED_TC
            Pair.of(
                    i -> {
                        String initiativeId = "INITIATIVEID" + i;
                        String userId = "USERID" + i;
                        EvaluationDTO out = new EvaluationDTO();
                        out.setUserId(userId);
                        out.setInitiativeId(initiativeId);
                        out.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
                        out.setAdmissibilityCheckDate(LocalDateTime.now());
                        out.setOnboardingRejectionReasons(List.of(
                                new OnboardingRejectionReason(
                                        "TYPE",
                                        "CODE",
                                        "AUTHORITY",
                                        "AUTHORITYLABEL",
                                        "DETAIL"
                                )
                        ));

                        storeOnboardingRequest(initiativeId, userId, OnboardingWorkflowConstants.INVITED);

                        return out;
                    },
                    o -> {
                        assertOnboardingUpdated(o, OnboardingWorkflowConstants.ONBOARDING_KO);
                        Assertions.assertEquals("CODE", o.getDetailKO());
                    }
            ),

            // useCase2: receiving DEMANDED on NOT existent onboarding request
            Pair.of(i -> {
                        String initiativeId = "INITIATIVEID" + i;
                        String userId = "USERID" + i;
                        EvaluationDTO out = new EvaluationDTO();
                        out.setUserId(userId);
                        out.setInitiativeId(initiativeId);
                        out.setStatus(OnboardingWorkflowConstants.DEMANDED);

                        return out;
                    },
                    o -> assertOnboardingUpdated(o, OnboardingWorkflowConstants.DEMANDED)
            ),

        // useCase3: as useCase2 but with RequestRateTooLarge exception
        Pair.of(
            i -> {
              String initiativeId = "INITIATIVEID" + i;
              String userId = "USERID" + i;
              EvaluationDTO out = new EvaluationDTO();
              out.setUserId(userId);
              out.setInitiativeId(initiativeId);
              out.setStatus(OnboardingWorkflowConstants.DEMANDED);

              String onboardingId = Onboarding.buildId(initiativeId, userId);

              buildOnboardingRepositoryRetryableStub(1).findByIdRetryable(onboardingId);
              buildOnboardingRepositoryRetryableStub(2).saveRetryable(Mockito.argThat(o -> o.getId().equals(onboardingId)));

              return out;
            },
            o -> {
              assertOnboardingUpdated(o, OnboardingWorkflowConstants.DEMANDED);
              Mockito.verify(onboardingRepositorySpy, Mockito.times(2)).findByIdRetryable(o.getId());
              Mockito.verify(onboardingRepositorySpy, Mockito.times(3)).saveRetryable(Mockito.argThat(s -> s.getId().equals(o.getId())));
            }
        )
    );

  private OnboardingRepository buildOnboardingRepositoryRetryableStub(int maxRetry) {
    int[] counter = {0};
    return Mockito.doAnswer(a -> {
      if(counter[0]++ < maxRetry){
        throw MongoRequestRateTooLargeRetryableTest.buildRequestRateTooLargeMongodbException();
      }else {
        return a.callRealMethod();
      }
    }).when(onboardingRepositorySpy);

  }

  private static void assertOnboardingUpdated(Onboarding result, String expectedStatus) {
        Assertions.assertEquals(expectedStatus, result.getStatus());
        Assertions.assertNotNull(result.getUpdateDate());
        Assertions.assertFalse(result.getCreationDate().isAfter(result.getUpdateDate()));
    }
    //endregion
}
