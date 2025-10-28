package it.gov.pagopa.onboarding.workflow.repository;

import com.mongodb.client.result.UpdateResult;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.model.Onboarding.Fields;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
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
  public UpdateResult disableAllFamilyMembers(String initiativeId, String familyId, LocalDateTime deactivationTime){
    Query query = Query.query(Criteria
            .where(Fields.initiativeId).is(initiativeId)
            .and(Fields.familyId).is(familyId)
    );

    Update update = new Update()
            .set(Fields.status, OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED)
            .set(Fields.requestDeactivationDate, deactivationTime)
            .set(Fields.updateDate, deactivationTime);

    return mongoTemplate.updateMulti(
            query,
            update,
            Onboarding.class
    );
  }

}
