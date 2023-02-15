package it.gov.pagopa.onboarding.workflow.utils;

import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.time.LocalDateTime;
import java.util.logging.Logger;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class Utilities {
  private static final String SRCIP;

  static {
    try {
      SRCIP = InetAddress.getLocalHost().getHostAddress();
    } catch (UnknownHostException e) {
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(), e.getMessage());
    }
  }

  private static final String CEF = String.format("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s", SRCIP);
  private static final String MSG = " msg=";
  private static final String USER = "suser=";
  private static final String INITIATIVE_ID = "cs1Label=initiativeId cs1=";
  private static final String CHANNEL = "cs2Label=channel cs2=";
  private static final String DATE = "cs3Label=date cs3=";
  final Logger logger = Logger.getLogger("AUDIT");


  private String buildLog(String eventLog, String userId, String initiativeId, String channel) {
    return CEF + MSG + eventLog + " " + USER + userId + " " + INITIATIVE_ID + initiativeId + " " + CHANNEL + channel;
  }

  private String buildLogWithDate(String eventLog, String userId, String initiativeId, String channel, LocalDateTime date){
    return buildLog(eventLog, userId, initiativeId, channel) + " " + DATE + date;
  }

  public void logTC(String userId, String initiativeId, String channel) {
    String testLog = this.buildLog("Terms and conditions accepted by the citizen", userId,
        initiativeId, channel);
    logger.info(testLog);
  }

  public void logTCIdemp(String userId, String initiativeId, String channel) {
    String testLog = this.buildLog("Terms and conditions already accepted by the citizen", userId,
        initiativeId, channel);
    logger.info(testLog);
  }

  public void logTCNotAccepted(String userId, String initiativeId, String channel) {
    String testLog = this.buildLog("Terms and conditions not accepted by the citizen", userId,
            initiativeId, channel);
    logger.info(testLog);
  }

  public void logPDND(String userId, String initiativeId, String channel) {
    String testLog = this.buildLog("Prerequisites required passed", userId, initiativeId, channel);
    logger.info(testLog);
  }

  public void logGetListPDND(String initiativeId) {
    String testLog = CEF + MSG + String.format("Retrieved PDND data about %s", initiativeId);
    logger.info(testLog);
  }

  public void logOnboardingComplete(String userId, String initiativeId, String channel, LocalDateTime date) {
    String testLog = this.buildLogWithDate("Onboarding of the citizen complete", userId, initiativeId, channel, date);
    logger.info(testLog);
  }

  public void logOnboardingOnEvaluation(String userId, String initiativeId, String channel, LocalDateTime date){
    String testLog = this.buildLogWithDate("Onboarding of the citizen on evaluation", userId, initiativeId, channel, date);
    logger.info(testLog);
  }

  public void logOnboardingKOWithReason(String userId, String initiativeId, String channel, String msg){
    String testLog = this.buildLog("Onboarding of the citizen failed: " + msg, userId, initiativeId, channel);
    logger.info(testLog);
  }

  public void logOnboardingKOInitiativeId(String initiativeId, String msg){
    String testLog = CEF + MSG + "Onboarding failed: " + msg + INITIATIVE_ID + initiativeId;
    logger.info(testLog);
  }

  public void logOnboardingKOWhiteList(String userId, String initiativeId, String channel, LocalDateTime date){
    String testLog = this.buildLogWithDate("Onboarding failed because the citizen is not allowed to participate to this initiative", userId, initiativeId, channel, date);
    logger.info(testLog);
  }

  public void logRollback(String userId, String initiativeId, String channel) {
    String testLog = this.buildLog("Onboarding rollback complete", userId, initiativeId, channel);
    logger.info(testLog);
  }

  public void logDeactivate(String userId, String initiativeId, String channel, LocalDateTime date) {
    String testLog = this.buildLogWithDate("Onboarding disabled", userId, initiativeId, channel, date);
    logger.info(testLog);
  }


}