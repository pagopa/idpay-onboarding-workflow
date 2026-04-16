package it.gov.pagopa.onboarding.workflow.repository;

import com.mongodb.bulk.BulkWriteResult;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.springframework.data.mongodb.core.query.Criteria;

import java.time.Instant;
import java.util.List;


public interface OnboardingSpecificRepository {
  List<Onboarding> findByFilter(Criteria criteria);
  Criteria getCriteria(String initiativeId, String userId, String status,
      Instant startDate, Instant endDate);
  long getCount(Criteria criteria);
  List<Onboarding> deletePaged(String initiativeId, int pageSize);

  BulkWriteResult disableAllFamilyMembers(String initiativeId, String userId, String familyId, Instant deactivationTime, Boolean updateFamilyMembers);

  BulkWriteResult reactivateAllFamilyMembers(String initiativeId, String userId, String familyId, Instant onboardingOkDate, Boolean updateFamilyMembers);
}
