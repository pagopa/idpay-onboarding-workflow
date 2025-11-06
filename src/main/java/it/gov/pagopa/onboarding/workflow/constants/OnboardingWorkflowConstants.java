package it.gov.pagopa.onboarding.workflow.constants;

import java.util.List;

public class OnboardingWorkflowConstants {

  public static final String ACCEPTED_TC = "ACCEPTED_TC";
  public static final String STATUS_UNSUBSCRIBED = "UNSUBSCRIBED";
  public static final String ON_EVALUATION = "ON_EVALUATION";
  public static final String ON_WAITING_LIST = "ON_WAITING_LIST";
  public static final String INVITED = "INVITED";
  public static final String DEMANDED = "DEMANDED";
  public static final String JOINED = "JOINED";
  public static final String SUSPENDED = "SUSPENDED";
  public static final String PUBLISHED = "PUBLISHED";
  public static final String ONBOARDING_OK = "ONBOARDING_OK";
  public static final String ONBOARDING_KO = "ONBOARDING_KO";
  public static final String REJECTED = "REJECTED";
  public static final String ELIGIBLE_KO = "ELIGIBLE_KO";
  public static final String BENEFICIARY_TYPE_NF = "NF";
  public static final String ALLOWED_CITIZEN_PUBLISH = "ALLOWED_CITIZEN_PUBLISH";
  public static final String OUT_OF_RANKING = "OUT_OF_RANKING";
  public static final String GENERIC_ERROR = "GENERIC_ERROR";
  public static final String TECHNICAL_ERROR_MSG_AUDIT = "Onboarding to initiative failed";
  public static final String GENERIC_REJECTION_ERROR_MSG_AUDIT = "The current citizen does not satisfy the criteria set for the initiative [%s]";
  public static final String ERROR_SELF_DECLARATION_SIZE_AUDIT = "The amount of self-declaration criteria inserted by the user does not match that required by the initiative";
  public static final String ERROR_SELF_DECLARATION_DENY_AUDIT = "The self-declaration criteria are not valid";
  public static final String ERROR_WHITELIST_MSG_AUDIT = "The current user is not allowed to participate to the initiative";
  public static final String ERROR_PDND_AUDIT = "The PDND consent was denied by the user for the initiative";
  public static final String ERROR_UNSUBSCRIBED_INITIATIVE_AUDIT = "Unsubscribed to initiative";
  public static final String ERROR_INITIATIVE_NOT_STARTED_MSG_AUDIT = "The initiative has not yet begun";
  public static final String ERROR_INITIATIVE_END_MSG_AUDIT = "The opportunity to join the initiative has already ended";
  public static final String ERROR_BUDGET_TERMINATED_MSG_AUDIT = "The budget for this initiative is terminated";
  public static final List<String> STATUS_IDEMPOTENT = List.of(ONBOARDING_OK, INVITED, SUSPENDED, ON_EVALUATION, DEMANDED);
  public static final String REJECTION_REASON_BIRTHDATE_KO = "AUTOMATED_CRITERIA_BIRTHDATE_FAIL";
  // Onboarding detailKO
  public static final String ERROR_INITIATIVE_END = "INITIATIVE_END";
  public static final String ERROR_BUDGET_TERMINATED = "BUDGET_TERMINATED";
  public static final String ERROR_WHITELIST = "NOT_IN_WHITELIST";
  public static final String ERROR_TECHNICAL = "TECHNICAL_ERROR";
  public static final class ExceptionMessage {
    public static final String ERROR_DISABLING_STATUS_MSG = "It is not possible to disable the user on initiative [%s]";
    public static final String ERROR_SUSPENSION_STATUS_MSG = "It is not possible to suspend the user on initiative [%s]";
    public static final String ERROR_READMIT_STATUS_MSG = "It is not possible to readmit the user on initiative [%s]";
    public static final String ERROR_MAX_NUMBER_FOR_PAGE_MSG = "Max number for page allowed: 15";

    public static final String TECHNICAL_ERROR_MSG = "Onboarding to initiative [%s] failed";
    public static final String UNSATISFIED_REQUIREMENTS_MSG = "The current user does not satisfy the requirements set for the initiative [%s]";
    public static final String ERROR_BUDGET_TERMINATED_MSG = "Budget exhausted for initiative [%s]";
    public static final String ERROR_INITIATIVE_NOT_STARTED_MSG = "The initiative [%s] has not yet begun";
    public static final String ERROR_INITIATIVE_END_MSG = "The opportunity to join the initiative [%s] has already ended";
    public static final String ERROR_INITIATIVE_NOT_ACTIVE_MSG = "The initiative [%s] has not been published";
    public static final String ERROR_UNSUBSCRIBED_INITIATIVE_MSG = "The user has unsubscribed from initiative [%s]";
    public static final String ERROR_PDND_MSG = "The PDND consent was denied by the user for the initiative [%s]";
    public static final String ERROR_WHITELIST_MSG = "The current user is not allowed to participate to the initiative [%s]";
    public static final String ERROR_SELF_DECLARATION_NOT_VALID_MSG = "The self-declaration criteria are not valid or those inserted by the user do not match those required by the initiative [%s]";

    public static final String INITIATIVE_NOT_FOUND_MSG = "Cannot find initiative [%s]";
    public static final String ID_S_NOT_FOUND_MSG = "The current user is not onboarded on initiative [%s]";

    public static final String EMAIL_NOT_MATCHED_MSG = "Email and confirmation email do not match.";
    public static final String TOS_NOT_CONFIRMED_MSG = "Terms and Conditions not accepted.";
    public static final String PDV_DECRYPT_ERROR_MSG = "An error occurred during decrypt";
    public static final String ERROR_INITIATIVE_INVOCATION_MSG = "An error occurred in the microservice initiative";
    public static final String ERROR_ADMISSIBILITY_INVOCATION_MSG = "An error occurred in the microservice admissibility";
    public static final String ERROR_SUSPENSION_MSG = "Error while suspending the user on initiative [%s]";
    public static final String ERROR_READMISSION_MSG = "Error while readmitting the user on initiative [%s]";

    private ExceptionMessage() {}

  }

  public static final class ExceptionCode {
    public static final String INVALID_REQUEST = "ONBOARDING_INVALID_REQUEST";
    public static final String PAGE_SIZE_NOT_ALLOWED = "ONBOARDING_PAGE_SIZE_NOT_ALLOWED";
    public static final String DEACTIVATION_NOT_ALLOWED = "ONBOARDING_DEACTIVATION_NOT_ALLOWED_FOR_USER_STATUS";
    public static final String SUSPENSION_NOT_ALLOWED = "ONBOARDING_SUSPENSION_NOT_ALLOWED_FOR_USER_STATUS";
    public static final String READMISSION_NOT_ALLOWED = "ONBOARDING_READMISSION_NOT_ALLOWED_FOR_USER_STATUS";

    public static final String USER_NOT_IN_WHITELIST = "ONBOARDING_USER_NOT_IN_WHITELIST";
    public static final String INITIATIVE_NOT_STARTED = "ONBOARDING_INITIATIVE_NOT_STARTED";
    public static final String INITIATIVE_ENDED = "ONBOARDING_INITIATIVE_ENDED";
    public static final String BUDGET_EXHAUSTED = "ONBOARDING_BUDGET_EXHAUSTED";
    public static final String INITIATIVE_NOT_PUBLISHED = "ONBOARDING_INITIATIVE_STATUS_NOT_PUBLISHED";
    public static final String USER_UNSUBSCRIBED = "ONBOARDING_USER_UNSUBSCRIBED";
    public static final String PDND_CONSENT_DENIED = "ONBOARDING_PDND_CONSENT_DENIED";
    public static final String SELF_DECLARATION_NOT_VALID = "ONBOARDING_SELF_DECLARATION_NOT_VALID";
    public static final String TECHNICAL_ERROR = "ONBOARDING_TECHNICAL_ERROR";
    public static final String UNSATISFIED_REQUIREMENTS = "ONBOARDING_UNSATISFIED_REQUIREMENTS";

    public static final String FAMILY_UNIT_ALREADY_JOINED = "ONBOARDING_FAMILY_UNIT_ALREADY_JOINED";
    public static final String WAITING_LIST = "ONBOARDING_WAITING_LIST";

    public static final String USER_NOT_ONBOARDED = "ONBOARDING_USER_NOT_ONBOARDED";
    public static final String INITIATIVE_NOT_FOUND = "ONBOARDING_INITIATIVE_NOT_FOUND";

    public static final String TOO_MANY_REQUESTS = "ONBOARDING_TOO_MANY_REQUESTS";

    public static final String GENERIC_ERROR = "ONBOARDING_GENERIC_ERROR";

    public static final String TOS_NOT_CONFIRMED = "ONBOARDING_TOS_NOT_CONFIRMED";
    public static final String EMAIL_NOT_MATCHED = "ONBOARDING_EMAIL_NOT_MATCHED";

    public static final String ISEE_CODE = "isee";

    public static final String INTEGER_ONE = "1";

    private ExceptionCode() {}
  }

  private OnboardingWorkflowConstants() {
  }
}
