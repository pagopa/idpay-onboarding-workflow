package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.SelfDeclarationText;

import java.util.List;


public interface SelfDeclarationTextSpecificRepository {
  List<SelfDeclarationText> deletePaged(String initiativeId, int pageSize);

}
