package it.gov.pagopa.onboarding.workflow.enums;

import lombok.Getter;

@Getter
public enum SelfCriteriaBooleanTypeCode {
  ISEE("ISEE");

  private final String description;

  SelfCriteriaBooleanTypeCode(String description) {
    this.description = description;
  }

}
