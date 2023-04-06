package it.gov.pagopa.onboarding.workflow.utils;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.time.LocalDateTime;

@Component
@AllArgsConstructor
@Slf4j(topic = "AUDIT")
public class AuditUtilities {
  public static final String SRCIP;

  static {
    String srcIp;
    try {
      srcIp = InetAddress.getLocalHost().getHostAddress();
    } catch (UnknownHostException e) {
      log.error("Cannot determine the ip of the current host", e);
      srcIp = "UNKNOWN";
    }
    SRCIP = srcIp;
  }

  private static final String CEF = String.format("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s", SRCIP);

  private static final String CEF_PATTERN_MSG = CEF + " msg={}";
  private static final String CEF_PATTERN_BASE = CEF_PATTERN_MSG + " suser={} cs1Label=initiativeId cs1={}";
  private static final String CEF_PATTERN = CEF_PATTERN_BASE + " cs2Label=channel cs2={}";
  private static final String CEF_DATE_PATTERN = CEF_PATTERN + " cs3Label=date cs3={}";

  private void logAuditString(String pattern, String... parameters) {
    log.info(pattern, (Object[]) parameters);
  }

  public void logTC(String userId, String initiativeId, String channel) {
    logAuditString(
            CEF_PATTERN,
            "Terms and conditions accepted by the citizen", userId, initiativeId, channel
    );
  }

  public void logTCIdemp(String userId, String initiativeId, String channel) {
    logAuditString(
            CEF_PATTERN,
            "Terms and conditions already accepted by the citizen", userId, initiativeId, channel
    );
  }

  public void logTCNotAccepted(String userId, String initiativeId, String channel) {
    logAuditString(
            CEF_PATTERN,
            "Terms and conditions not accepted by the citizen", userId, initiativeId, channel
    );
  }

  public void logPDND(String userId, String initiativeId, String channel) {
    logAuditString(
            CEF_PATTERN,
            "Prerequisites required passed", userId, initiativeId, channel
    );
  }

  public void logGetListPDND(String initiativeId) {
    logAuditString(
            CEF_PATTERN_MSG,
            String.format("Retrieved PDND data about %s", initiativeId)
    );
  }

  public void logOnboardingComplete(String userId, String initiativeId, String channel, LocalDateTime date) {
    logAuditString(
            CEF_DATE_PATTERN,
            "Onboarding of the citizen complete", userId, initiativeId, channel, date.toString()
    );
  }

  public void logOnboardingOnEvaluation(String userId, String initiativeId, String channel, LocalDateTime date) {
    logAuditString(
            CEF_DATE_PATTERN,
            "Onboarding of the citizen on evaluation", userId, initiativeId, channel, date.toString()
    );
  }

  public void logOnboardingKOWithReason(String userId, String initiativeId, String channel, String msg) {
    logAuditString(
            CEF_PATTERN,
            String.format("Onboarding of the citizen failed: %s", msg), userId, initiativeId, channel
    );
  }

  public void logOnboardingKOInitiativeId(String initiativeId, String msg) {
    logAuditString(
            CEF_PATTERN_MSG,
            String.format("Onboarding failed for initiative %s: %s", initiativeId, msg)
    );
  }

  public void logOnboardingKOWhiteList(String userId, String initiativeId, String channel, LocalDateTime date) {
    logAuditString(
            CEF_DATE_PATTERN,
            "Onboarding failed because the citizen is not allowed to participate to this initiative",
            userId, initiativeId, channel, date.toString()
    );
  }

  public void logRollback(String userId, String initiativeId, String channel) {
    logAuditString(
            CEF_PATTERN,
            "Onboarding rollback complete", userId, initiativeId, channel
    );
  }

  public void logDeactivate(String userId, String initiativeId, String channel, LocalDateTime date) {
    logAuditString(
            CEF_DATE_PATTERN,
            "Onboarding disabled", userId, initiativeId, channel, date.toString()
    );
  }

  public void logSuspension(String userId, String initiativeId) {
    logAuditString(
            CEF_PATTERN_BASE,
            "Onboarding suspended", userId, initiativeId
    );
  }

  public void logSuspensionKO(String userId, String initiativeId) {
    logAuditString(
            CEF_PATTERN_BASE,
            "Onboarding suspension failed", userId, initiativeId
    );
  }

  public void logReadmission(String userId, String initiativeId) {
    logAuditString(
            CEF_PATTERN_BASE,
            "Onboarding readmitted", userId, initiativeId
    );
  }

  public void logReadmissionKO(String userId, String initiativeId) {
    logAuditString(
            CEF_PATTERN_BASE,
            "Onboarding readmission failed", userId, initiativeId
    );
  }
}