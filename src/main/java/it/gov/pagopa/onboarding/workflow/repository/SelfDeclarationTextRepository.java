package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.SelfDeclarationText;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface SelfDeclarationTextRepository extends MongoRepository<SelfDeclarationText, String> {
}
