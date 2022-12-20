package it.gov.pagopa.onboarding.workflow.constants;

public final class OnboardingWorkflowConstants {

  public static final String ACCEPTED_TC = "ACCEPTED_TC";
  public static final String STATUS_UNSUBSCRIBED = "UNSUBSCRIBED";
  public static final String ON_EVALUATION = "ON_EVALUATION";
  public static final String INVITED = "INVITED";
  public static final String ONBOARDING_OK = "ONBOARDING_OK";
  public static final String ONBOARDING_KO = "ONBOARDING_KO";
  public static final String ELEGIBILE_KO = "ELEGIBILE_KO";
  public static final String ALLOWED_CITIZEN_PUBLISH = "ALLOWED_CITIZEN_PUBLISH";
  public static final String OUT_OF_RANKING = "OUT_OF_RANKING";
  public static final String ID_S_NOT_FOUND = "Onboarding with initiativeId %s and current userId not found.";
  public static final String ERROR_PREREQUISITES = "The initiative has not met the prerequisites.";
  public static final String ERROR_SELF_DECLARATION_SIZE = "The amount of self declaration lists mismatch the amount of flags";
  public static final String ERROR_SELF_DECLARATION_DENY = "The selfDeclarationList was denied by the user for the initiative %s.";
  public static final String ERROR_WHITELIST = "The citizen is not allowed to participate to this initiative!";

  private OnboardingWorkflowConstants() {
  }
}
