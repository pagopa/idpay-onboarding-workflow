package it.gov.pagopa.onboarding.workflow.enums;

import lombok.Getter;

@Getter
public enum SelfCriteriaMultiTypeCode {
  ISEE("ISEE");

  private final String description;

  SelfCriteriaMultiTypeCode(String description) {
    this.description = description;
  }

}
