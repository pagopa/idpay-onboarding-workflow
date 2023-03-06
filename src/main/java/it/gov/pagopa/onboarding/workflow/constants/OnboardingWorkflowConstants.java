package it.gov.pagopa.onboarding.workflow.constants;

public final class OnboardingWorkflowConstants {

  public static final String ACCEPTED_TC = "ACCEPTED_TC";
  public static final String STATUS_UNSUBSCRIBED = "UNSUBSCRIBED";
  public static final String ON_EVALUATION = "ON_EVALUATION";
  public static final String INVITED = "INVITED";
  public static final String PUBLISHED = "PUBLISHED";
  public static final String ONBOARDING_OK = "ONBOARDING_OK";
  public static final String ONBOARDING_KO = "ONBOARDING_KO";
  public static final String ELIGIBLE_KO = "ELIGIBLE_KO";
  public static final String ALLOWED_CITIZEN_PUBLISH = "ALLOWED_CITIZEN_PUBLISH";
  public static final String OUT_OF_RANKING = "OUT_OF_RANKING";
  public static final String ID_S_NOT_FOUND = "Onboarding with initiativeId %s and current userId not found.";
  public static final String ERROR_SELF_DECLARATION_SIZE = "The amount of self declaration lists mismatch the amount of flags";
  public static final String ERROR_SELF_DECLARATION_DENY = "The selfDeclarationList was denied by the user for the initiative %s.";
  public static final String ERROR_WHITELIST = "The citizen is not allowed to participate to this initiative!";
  public static final String ERROR_TC = "Terms and Conditions have been not accepted by the current user for initiative %s.";
  public static final String ERROR_MAX_NUMBER_FOR_PAGE = "Max number for page allowed: 15";
  public static final String ERROR_PDND = "The PDND consent was denied by the user for the initiative %s.";
  public static final String ERROR_UNSUBSCRIBED_INITIATIVE = "Unsubscribed to initiative";
  public static final String ERROR_INITIATIVE_NOT_ACTIVE = "The initiative is not active!";
  public static final String ERROR_INITIATIVE_NOT_STARTED = "INITIATIVE_NOT_STARTED";
  public static final String ERROR_INITIATIVE_NOT_STARTED_MSG = "The initiative has not yet begun";
  public static final String ERROR_INITIATIVE_END = "INITIATIVE_END";
  public static final String ERROR_INITIATIVE_END_MSG = "The opportunity to join the initiative has already ended";
  public static final String ERROR_BUDGET_TERMINATED = "BUDGET_TERMINATED";
  public static final String ERROR_BUDGET_TERMINATED_MSG = "The budget for this initiative is terminated";
  public static final String ERROR_INITIATIVE_SUSPENDED = "INITIATIVE_SUSPENDED";
  public static final String ERROR_INITIATIVE_SUSPENDED_MSG = "The initiative is suspended";
  public static final String GENERIC_ERROR = "GENERIC_ERROR";
  public static final String GENERIC_ERROR_MSG = "Onbording to initiative failed";

  private OnboardingWorkflowConstants() {
  }
}
