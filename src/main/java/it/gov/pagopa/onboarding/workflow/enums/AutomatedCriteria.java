package it.gov.pagopa.onboarding.workflow.enums;

import lombok.Getter;

@Getter
public enum AutomatedCriteria {
  ISEE("ISEE"),
  BIRTHDATE("Anno di nascita"),
  RESIDENCE("Città di residenza");

  private final String description;

  AutomatedCriteria(String description) {
    this.description = description;
  }

}
