package it.gov.pagopa.onboarding.workflow.utils;

import ch.qos.logback.classic.LoggerContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;

import java.time.LocalDateTime;

class AuditUtilitiesTest {

  private static final String MSG = "TEST_MSG";
  private static final String CHANNEL = "CHANNEL";
  private static final String USER_ID = "TEST_USER_ID";
  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private static final LocalDateTime DATE = LocalDateTime.now();

  private final AuditUtilities auditUtilities = new AuditUtilities();
  private MemoryAppender memoryAppender;

  @BeforeEach
  public void setup() {
    ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger("AUDIT");
    memoryAppender = new MemoryAppender();
    memoryAppender.setContext((LoggerContext) LoggerFactory.getILoggerFactory());
    logger.setLevel(ch.qos.logback.classic.Level.INFO);
    logger.addAppender(memoryAppender);
    memoryAppender.start();
  }
  private void checkCommonFields() {
    Assertions.assertTrue(memoryAppender.contains(ch.qos.logback.classic.Level.INFO,USER_ID));
    Assertions.assertTrue(memoryAppender.contains(ch.qos.logback.classic.Level.INFO,CHANNEL));
    Assertions.assertTrue(memoryAppender.contains(ch.qos.logback.classic.Level.INFO,INITIATIVE_ID));

    Assertions.assertEquals(1, memoryAppender.getLoggedEvents().size());
  }

  @Test
  void logTC_ok() {
    auditUtilities.logTC(USER_ID, INITIATIVE_ID, CHANNEL);
    checkCommonFields();

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Terms and conditions accepted by the citizen" +
                    " suser=%s cs1Label=initiativeId cs1=%s cs2Label=channel cs2=%s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            USER_ID,
                            INITIATIVE_ID,
                            CHANNEL
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }
  @Test
  void logTCIdemp_ok() {
    auditUtilities.logTCIdemp(USER_ID, INITIATIVE_ID, CHANNEL);
    checkCommonFields();

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Terms and conditions already accepted by the citizen" +
                    " suser=%s cs1Label=initiativeId cs1=%s cs2Label=channel cs2=%s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            USER_ID,
                            INITIATIVE_ID,
                            CHANNEL
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }
  @Test
  void logTCNotAccepted_ok() {
    auditUtilities.logTCNotAccepted(USER_ID, INITIATIVE_ID, CHANNEL);
    checkCommonFields();

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Terms and conditions not accepted by the citizen" +
                    " suser=%s cs1Label=initiativeId cs1=%s cs2Label=channel cs2=%s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            USER_ID,
                            INITIATIVE_ID,
                            CHANNEL
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }
  @Test
  void logPDND_ok() {
    auditUtilities.logPDND(USER_ID, INITIATIVE_ID, CHANNEL);
    checkCommonFields();

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Prerequisites required passed" +
                    " suser=%s cs1Label=initiativeId cs1=%s cs2Label=channel cs2=%s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            USER_ID,
                            INITIATIVE_ID,
                            CHANNEL
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }
  @Test
  void logGetListPDND_ok() {
    auditUtilities.logGetListPDND(INITIATIVE_ID);

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Retrieved PDND data about %s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            INITIATIVE_ID
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }
  @Test
  void logOnboardingComplete_ok() {
    auditUtilities.logOnboardingComplete(USER_ID, INITIATIVE_ID, CHANNEL, DATE);
    checkCommonFields();

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Onboarding of the citizen complete" +
                    " suser=%s cs1Label=initiativeId cs1=%s cs2Label=channel cs2=%s cs3Label=date cs3=%s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            USER_ID,
                            INITIATIVE_ID,
                            CHANNEL,
                            DATE
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }
  @Test
  void logOnboardingOnEvaluation_ok() {
    auditUtilities.logOnboardingOnEvaluation(USER_ID, INITIATIVE_ID, CHANNEL, DATE);
    checkCommonFields();

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Onboarding of the citizen on evaluation" +
                    " suser=%s cs1Label=initiativeId cs1=%s cs2Label=channel cs2=%s cs3Label=date cs3=%s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            USER_ID,
                            INITIATIVE_ID,
                            CHANNEL,
                            DATE
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }
  @Test
  void logOnboardingKOWithReason_ok() {
    auditUtilities.logOnboardingKOWithReason(USER_ID, INITIATIVE_ID, CHANNEL, MSG);
    checkCommonFields();

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Onboarding of the citizen failed: %s" +
                    " suser=%s cs1Label=initiativeId cs1=%s cs2Label=channel cs2=%s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            MSG,
                            USER_ID,
                            INITIATIVE_ID,
                            CHANNEL
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }
  @Test
  void logOnboardingKOInitiativeId_ok() {
    auditUtilities.logOnboardingKOInitiativeId(INITIATIVE_ID, MSG);

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Onboarding failed for initiative %s: %s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            INITIATIVE_ID,
                            MSG
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }
  @Test
  void logOnboardingKOWhiteList_ok() {
    auditUtilities.logOnboardingKOWhiteList(USER_ID, INITIATIVE_ID, CHANNEL, DATE);
    checkCommonFields();

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Onboarding failed because the citizen is not allowed " +
                    "to participate to this initiative suser=%s cs1Label=initiativeId cs1=%s cs2Label=channel cs2=%s cs3Label=date cs3=%s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            USER_ID,
                            INITIATIVE_ID,
                            CHANNEL,
                            DATE
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }
  @Test
  void logRollback_ok() {
    auditUtilities.logRollback(USER_ID, INITIATIVE_ID, CHANNEL);
    checkCommonFields();

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Onboarding rollback complete" +
                    " suser=%s cs1Label=initiativeId cs1=%s cs2Label=channel cs2=%s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            USER_ID,
                            INITIATIVE_ID,
                            CHANNEL
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }@Test
  void logDeactivate_ok() {
    auditUtilities.logDeactivate(USER_ID, INITIATIVE_ID, CHANNEL, DATE);
    checkCommonFields();

    Assertions.assertEquals(
            ("CEF:0|PagoPa|IDPAY|1.0|7|User interaction|2| event=Onboarding dstip=%s msg=Onboarding disabled" +
                    " suser=%s cs1Label=initiativeId cs1=%s cs2Label=channel cs2=%s cs3Label=date cs3=%s")
                    .formatted(
                            AuditUtilities.SRCIP,
                            USER_ID,
                            INITIATIVE_ID,
                            CHANNEL,
                            DATE
                    ),
            memoryAppender.getLoggedEvents().get(0).getFormattedMessage()
    );
  }
}
