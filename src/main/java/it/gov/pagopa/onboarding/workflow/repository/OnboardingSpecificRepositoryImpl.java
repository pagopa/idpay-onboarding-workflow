package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.model.Onboarding.Fields;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

public class OnboardingSpecificRepositoryImpl implements OnboardingSpecificRepository {

  private final MongoTemplate mongoTemplate;

  public OnboardingSpecificRepositoryImpl(MongoTemplate mongoTemplate) {
    this.mongoTemplate = mongoTemplate;
  }

@Override
  public List<Onboarding> findByFilter(Criteria criteria, Pageable pageable) {
    return mongoTemplate.find(
        Query.query(criteria)
            .with(this.getPageable(pageable)),
        Onboarding.class);
  }

  @Override
  public long getCount(Criteria criteria){
    Query query = new Query();
    query.addCriteria(criteria);
    return mongoTemplate.count(query, Onboarding.class);
  }

  public Criteria getCriteria(String initiativeId, String userId, String status,
      LocalDateTime startDate, LocalDateTime endDate) {

    Criteria criteria = Criteria.where(Onboarding.Fields.initiativeId).is(initiativeId);
    if (userId != null) {
      criteria.and(Onboarding.Fields.userId).is(userId);
    }
    if (status != null) {
      if (List.of(OnboardingWorkflowConstants.ACCEPTED_TC, OnboardingWorkflowConstants.INVITED,
              OnboardingWorkflowConstants.ON_EVALUATION).contains(status)) {
        criteria.orOperator(Criteria.where(Fields.status).is(OnboardingWorkflowConstants.INVITED),
                Criteria.where(Fields.status).is(OnboardingWorkflowConstants.ACCEPTED_TC),
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

  private Pageable getPageable(Pageable pageable) {
    if (pageable == null) {
      return PageRequest.of(0, 15, Sort.by("lastUpdate"));
    }
    return pageable;
  }
}
