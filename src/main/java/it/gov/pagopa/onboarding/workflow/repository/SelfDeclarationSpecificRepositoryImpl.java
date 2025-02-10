package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding.Fields;
import it.gov.pagopa.onboarding.workflow.model.SelfDeclarationText;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

import java.util.List;

public class SelfDeclarationSpecificRepositoryImpl implements SelfDeclarationTextSpecificRepository {

  private final MongoTemplate mongoTemplate;

  public SelfDeclarationSpecificRepositoryImpl(MongoTemplate mongoTemplate) {
    this.mongoTemplate = mongoTemplate;
  }

  @Override
  public List<SelfDeclarationText> deletePaged(String initiativeId, int pageSize){
    Pageable pageable = PageRequest.of(0, pageSize);
    return mongoTemplate.findAllAndRemove(
            Query.query(Criteria.where(Fields.initiativeId).is(initiativeId)).with(pageable),
            SelfDeclarationText.class
    );
  }

}
