package it.gov.pagopa.onboarding.workflow.repository;

import com.mongodb.bulk.BulkWriteResult;
import com.mongodb.client.result.UpdateResult;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.data.mongodb.core.query.Criteria;

import java.time.LocalDateTime;
import java.util.List;


public interface OnboardingSpecificRepository {
  List<Onboarding> findByFilter(Criteria criteria);
  Criteria getCriteria(String initiativeId, String userId, String status,
      LocalDateTime startDate, LocalDateTime endDate);
  long getCount(Criteria criteria);
  List<Onboarding> deletePaged(String initiativeId, int pageSize);

  BulkWriteResult disableAllFamilyMembers(String initiativeId, String userId, String familyId, LocalDateTime deactivationTime, Boolean updateFamilyMembers);

  BulkWriteResult reactivateAllFamilyMembers(String initiativeId, String userId, String familyId, LocalDateTime onboardingOkDate, Boolean updateFamilyMembers);
}
