package it.gov.pagopa.onboarding.workflow.event.consumer;

import it.gov.pagopa.common.mongo.retry.MongoRequestRateTooLargeRetryerTest;
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
import java.util.Optional;
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
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.util.CollectionUtils;

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

              buildMongoRepositoryRetryableStub(1, (String id) -> alternativeFindById(onboardingRepositorySpy, id)).findById(onboardingId);
              buildMongoRepositoryRetryableStub(2, (Onboarding o) -> alternativeSave(onboardingRepositorySpy, o)).save(Mockito.argThat(o -> o.getId().equals(onboardingId)));

              return out;
            },
            o -> {
              assertOnboardingUpdated(o, OnboardingWorkflowConstants.DEMANDED);
              Mockito.verify(onboardingRepositorySpy, Mockito.times(2)).findById(o.getId());
              Mockito.verify(onboardingRepositorySpy, Mockito.times(3)).save(Mockito.argThat(s -> s.getId().equals(o.getId())));
            }
        )
    );

  private <T> OnboardingRepository buildMongoRepositoryRetryableStub(int maxRetry, Function<T, ?> realMethod) {
    int[] counter = {0};
    return Mockito.doAnswer(a -> {
      if(counter[0]++ < maxRetry){
        throw MongoRequestRateTooLargeRetryerTest.buildRequestRateTooLargeMongodbException_whenReading();
      }else {
        return realMethod.apply(a.getArgument(0));
      }
    }).when(onboardingRepositorySpy);

  }

  private <T> Optional<T> alternativeFindById(MongoRepository<T, String> mongoRepository, String id){
    final List<T> result = mongoRepository.findAllById(List.of(id));
    if(!CollectionUtils.isEmpty(result)){
      return Optional.of(result.get(0));
    }else {
      return Optional.empty();
    }
  }

  private <T> T alternativeSave(MongoRepository<T, String> mongoRepository, T entity){
    final List<T> result = mongoRepository.saveAll(List.of(entity));
    if(!CollectionUtils.isEmpty(result)){
      return result.get(0);
    }else {
      return null;
    }
  }

  private static void assertOnboardingUpdated(Onboarding result, String expectedStatus) {
        Assertions.assertEquals(expectedStatus, result.getStatus());
        Assertions.assertNotNull(result.getUpdateDate());
        Assertions.assertFalse(result.getCreationDate().isAfter(result.getUpdateDate()));
    }
    //endregion
}
