package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.model.Onboarding.Fields;
import java.time.LocalDateTime;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import reactor.core.publisher.Flux;

public class OnboardingSpecificRepositoryImpl implements OnboardingSpecificRepository{
  private final ReactiveMongoTemplate mongoTemplate;

  public OnboardingSpecificRepositoryImpl(ReactiveMongoTemplate mongoTemplate) {
    this.mongoTemplate = mongoTemplate;
  }
  @Override
  public Flux<Onboarding> findByFilter(String initiativeId, String userId, String status,
      LocalDateTime startDate, LocalDateTime endDate, Pageable pageable) {
    Criteria criteria = Criteria.where(Onboarding.Fields.initiativeId).is(initiativeId);
    if (userId != null) {
      criteria.and(Onboarding.Fields.userId).is(userId);
    }
    if (status != null) {
      criteria.and(Onboarding.Fields.status).is(status);
      if (startDate != null && endDate != null) {
        switch (status) {
          case OnboardingWorkflowConstants.ONBOARDING_OK:
            criteria.and(Fields.OnboardingOkDate).gte(startDate).lte(endDate);
            break;
          case OnboardingWorkflowConstants.ACCEPTED_TC:
            criteria.and(Fields.tcAcceptTimestamp).gte(startDate).lte(endDate);
            break;
          case OnboardingWorkflowConstants.ON_EVALUATION:
            criteria.and(Fields.criteriaConsensusTimestamp).gte(startDate).lte(endDate);
            break;
          case OnboardingWorkflowConstants.STATUS_INACTIVE:
            criteria.and(Fields.requestDeactivationDate).gte(startDate).lte(endDate);
            break;
          case OnboardingWorkflowConstants.INVITED:
            criteria.and(Fields.invitationTimestamp).gte(startDate).lte(endDate);
            break;
        }
      } else if (startDate != null) {
        switch (status) {
          case OnboardingWorkflowConstants.ONBOARDING_OK:
            criteria.and(Fields.OnboardingOkDate).gte(startDate);
            break;
          case OnboardingWorkflowConstants.ACCEPTED_TC:
            criteria.and(Fields.tcAcceptTimestamp).gte(startDate);
            break;
          case OnboardingWorkflowConstants.ON_EVALUATION:
            criteria.and(Fields.criteriaConsensusTimestamp).gte(startDate);
            break;
          case OnboardingWorkflowConstants.STATUS_INACTIVE:
            criteria.and(Fields.requestDeactivationDate).gte(startDate);
            break;
          case OnboardingWorkflowConstants.INVITED:
            criteria.and(Fields.invitationTimestamp).gte(startDate);
            break;
        }
      } else if (endDate != null) {
        switch (status) {
          case OnboardingWorkflowConstants.ONBOARDING_OK:
            criteria.and(Fields.OnboardingOkDate).lte(endDate);
            break;
          case OnboardingWorkflowConstants.ACCEPTED_TC:
            criteria.and(Fields.tcAcceptTimestamp).lte(endDate);
            break;
          case OnboardingWorkflowConstants.ON_EVALUATION:
            criteria.and(Fields.criteriaConsensusTimestamp).lte(endDate);
            break;
          case OnboardingWorkflowConstants.STATUS_INACTIVE:
            criteria.and(Fields.requestDeactivationDate).lte(endDate);
            break;
          case OnboardingWorkflowConstants.INVITED:
            criteria.and(Fields.invitationTimestamp).lte(endDate);
            break;
        }
      }
    }
    if(status==null){
      if(startDate != null && endDate != null){
        criteria.and(Onboarding.Fields.lastUpdate)
            .gte(startDate)
            .lte(endDate);
      }else if(startDate != null){
        criteria.and(Onboarding.Fields.lastUpdate)
            .gte(startDate);
      }else if(endDate != null){
        criteria.and(Onboarding.Fields.lastUpdate)
            .lte(endDate);
      }
    }
    return mongoTemplate.find(
        Query.query(criteria)
            .with(this.getPageable(pageable)),
        Onboarding.class);
  }
  private Pageable getPageable(Pageable pageable){
    if (pageable == null) {
      pageable = Pageable.unpaged();
    }
    return pageable;
  }
}
