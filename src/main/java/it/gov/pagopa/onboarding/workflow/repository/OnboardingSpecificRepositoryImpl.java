package it.gov.pagopa.onboarding.workflow.repository;

import com.mongodb.bulk.BulkWriteResult;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.model.Onboarding.Fields;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;

import java.time.LocalDateTime;
import java.util.List;

public class OnboardingSpecificRepositoryImpl implements OnboardingSpecificRepository {

  private final MongoTemplate mongoTemplate;

  public OnboardingSpecificRepositoryImpl(MongoTemplate mongoTemplate) {
    this.mongoTemplate = mongoTemplate;
  }

@Override
  public List<Onboarding> findByFilter(Criteria criteria) {
    return mongoTemplate.find(
        Query.query(criteria),
        Onboarding.class);
  }

  @Override
  public long getCount(Criteria criteria){
    Query query = new Query();
    query.addCriteria(criteria);
    return mongoTemplate.count(query, Onboarding.class);
  }

  @Override
  public List<Onboarding> deletePaged(String initiativeId, int pageSize){
    Pageable pageable = PageRequest.of(0, pageSize);
    return mongoTemplate.findAllAndRemove(
            Query.query(Criteria.where(Fields.initiativeId).is(initiativeId)).with(pageable),
            Onboarding.class
    );
  }

  public Criteria getCriteria(String initiativeId, String userId, String status,
      LocalDateTime startDate, LocalDateTime endDate) {

    Criteria criteria = Criteria.where(Onboarding.Fields.initiativeId).is(initiativeId);
    if (userId != null) {
      criteria.and(Onboarding.Fields.userId).is(userId);
    }
    if (status != null) {
      if (List.of(OnboardingWorkflowConstants.ACCEPTED_TC, OnboardingWorkflowConstants.INVITED,
              OnboardingWorkflowConstants.ON_EVALUATION, OnboardingWorkflowConstants.DEMANDED).contains(status)) {
        criteria.orOperator(Criteria.where(Fields.status).is(OnboardingWorkflowConstants.INVITED),
                Criteria.where(Fields.status).is(OnboardingWorkflowConstants.ACCEPTED_TC),
                Criteria.where(Fields.status).is(OnboardingWorkflowConstants.DEMANDED),
                Criteria.where(Fields.status).is(OnboardingWorkflowConstants.ON_EVALUATION));
      } else {
      criteria.and(Onboarding.Fields.status).is(status);
      }
    }
    if (startDate != null && endDate != null) {
      criteria.and(Onboarding.Fields.updateDate)
          .gte(startDate)
          .lte(endDate);
    } else if (startDate != null) {
      criteria.and(Onboarding.Fields.updateDate)
          .gte(startDate);
    } else if (endDate != null) {
      criteria.and(Onboarding.Fields.updateDate)
          .lte(endDate);
    }
    return criteria;
  }

  @Override
  public BulkWriteResult disableAllFamilyMembers(
          String initiativeId, String userId, String familyId, LocalDateTime deactivationTime, Boolean updateFamilyMembers){


    Update update = new Update()
            .set(Fields.status, OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED)
            .set(Fields.requestDeactivationDate, deactivationTime)
            .set(Fields.updateDate, deactivationTime);

    BulkOperations bulkOps = mongoTemplate.bulkOps(BulkOperations.BulkMode.UNORDERED, Onboarding.class);

    if (Boolean.TRUE.equals(updateFamilyMembers)) {
      Query familyQuery = Query.query(Criteria
              .where(Fields.initiativeId).is(initiativeId)
              .and(Fields.familyId).is(familyId)
      );
      familyQuery.fields().include("_id");

      List<String> familyMemberIds = mongoTemplate
              .find(familyQuery, Onboarding.class)
              .stream()
              .map(Onboarding::getId)
              .toList();

      for (String id : familyMemberIds) {
        bulkOps.updateOne(
                Query.query(Criteria.where(Fields.id).is(id)),
                update
        );
      }
    } else {
        bulkOps.updateOne(
                Query.query(Criteria.where(Fields.id).is(userId+"_"+initiativeId)),
                update
        );
    }

    return bulkOps.execute();
  }

  @Override
  public BulkWriteResult reactivateAllFamilyMembers(
          String initiativeId, String userId, String familyId, LocalDateTime onboardingOkDate, Boolean updateFamilyMembers) {

    BulkOperations bulkOps = mongoTemplate.bulkOps(BulkOperations.BulkMode.UNORDERED, Onboarding.class);

    Query queryOne = Query.query(Criteria
            .where(Fields.initiativeId).is(initiativeId)
            .and(Fields.userId).is(userId)
    );
    Update updateOne = new Update()
            .set(Fields.status, OnboardingWorkflowConstants.ONBOARDING_OK)
            .set(Fields.requestDeactivationDate, null)
            .set(Fields.updateDate, onboardingOkDate);
    bulkOps.updateOne(queryOne, updateOne);

    if (Boolean.TRUE.equals(updateFamilyMembers)) {
      Query queryMany = Query.query(Criteria
              .where(Fields.initiativeId).is(initiativeId)
              .and(Fields.familyId).is(familyId)
              .and(Fields.userId).ne(userId)
      );
      Update updateMany = new Update()
              .set(Fields.status, OnboardingWorkflowConstants.JOINED)
              .set(Fields.requestDeactivationDate, null)
              .set(Fields.updateDate, onboardingOkDate);
      bulkOps.updateMulti(queryMany, updateMany);
    }

    return bulkOps.execute();

  }

}
