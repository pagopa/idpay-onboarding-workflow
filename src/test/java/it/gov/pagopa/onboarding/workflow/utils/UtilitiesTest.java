package it.gov.pagopa.onboarding.workflow.utils;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import it.gov.pagopa.onboarding.workflow.exception.OnboardingWorkflowException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith({SpringExtension.class, MockitoExtension.class})
@ContextConfiguration(classes = {Utilities.class,InetAddress.class})
class UtilitiesTest {
  private static final String SRCIP;

  static {
    try {
      SRCIP = InetAddress.getLocalHost().getHostAddress();
    } catch (UnknownHostException e) {
      throw new OnboardingWorkflowException(HttpStatus.BAD_REQUEST.value(), e.getMessage());
    }
  }

  private static final String CEF = String.format("CEF:0 srcip=%s ", SRCIP);
  private static final String MSG = " TEST_MSG";
  private static final String CHANNEL = "CHANNEL";
  private static final String USER_ID = "TEST_USER_ID";
  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private static final String REASON_KO = "TEST_REASON_KO";
  private static final LocalDateTime DATE = LocalDateTime.now();

  @MockBean
  Logger logger;
  @Autowired
  Utilities utilities;
  @MockBean
  InetAddress inetAddress;
  MemoryAppender memoryAppender;

  @BeforeEach
  public void setup() {
    ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger("AUDIT");
    memoryAppender = new MemoryAppender();
    memoryAppender.setContext((LoggerContext) LoggerFactory.getILoggerFactory());
    logger.setLevel(ch.qos.logback.classic.Level.INFO);
    logger.addAppender(memoryAppender);
    memoryAppender.start();
  }


  @Test
  void logTC_ok(){
    utilities.logTC(USER_ID,INITIATIVE_ID, CHANNEL);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG,MSG)).isFalse();
  }

  @Test
  void logTC_ko() {
    Mockito.doThrow(new OnboardingWorkflowException(400,"")).when(inetAddress).getHostAddress();
    utilities.logTC(USER_ID,INITIATIVE_ID, CHANNEL);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG,MSG)).isFalse();
  }

  @Test
  void logTCIdemp_ok(){
    utilities.logTCIdemp(USER_ID,INITIATIVE_ID, CHANNEL);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG,MSG)).isFalse();
  }

  @Test
  void logTCNotAccepted_ok(){
    utilities.logTCNotAccepted(USER_ID,INITIATIVE_ID, CHANNEL);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG,MSG)).isFalse();
  }

  @Test
  void logPDND_ok(){
    utilities.logPDND(USER_ID,INITIATIVE_ID, CHANNEL);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG,MSG)).isFalse();
  }

  @Test
  void logGetListPDND_ok(){
    utilities.logGetListPDND(INITIATIVE_ID);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG,MSG)).isFalse();
  }

  @Test
  void logOnboardingOk_ok(){
    utilities.logOnboardingComplete(USER_ID,INITIATIVE_ID,CHANNEL, DATE);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG,MSG)).isFalse();
  }

  @Test
  void logOnboardingOnEvaluation_ok(){
    utilities.logOnboardingOnEvaluation(USER_ID, INITIATIVE_ID, CHANNEL, DATE);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG, MSG)).isFalse();
  }

  @Test
  void logOnboardingKOWithReason_ok(){
    utilities.logOnboardingKOWithReason(USER_ID, INITIATIVE_ID, CHANNEL, REASON_KO);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG, MSG)).isFalse();
  }

  @Test
  void logOnboardingKOWhiteList_ok(){
    utilities.logOnboardingKOWhiteList(USER_ID, INITIATIVE_ID, CHANNEL, DATE);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG, MSG)).isFalse();
  }

  @Test
  void logOnboardingKOInitiativeId_ok(){
    utilities.logOnboardingKOInitiativeId(INITIATIVE_ID, REASON_KO);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG, MSG)).isFalse();
  }

  @Test
  void logRollback_ok(){
    utilities.logRollback(USER_ID,INITIATIVE_ID,CHANNEL);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG,MSG)).isFalse();
  }

  @Test
  void logDeactivate_ok(){
    utilities.logDeactivate(USER_ID,INITIATIVE_ID,CHANNEL, DATE);
    assertThat(memoryAppender.contains(ch.qos.logback.classic.Level.DEBUG,MSG)).isFalse();
  }


  public static class MemoryAppender extends ListAppender<ILoggingEvent> {
    public void reset() {
      this.list.clear();
    }

    public boolean contains(ch.qos.logback.classic.Level level, String string) {
      return this.list.stream()
          .anyMatch(event -> event.toString().contains(string)
              && event.getLevel().equals(level));
    }

    public int countEventsForLogger(String loggerName) {
      return (int) this.list.stream()
          .filter(event -> event.getLoggerName().contains(loggerName))
          .count();
    }

    public List<ILoggingEvent> search() {
      return this.list.stream()
          .filter(event -> event.toString().contains(MSG))
          .collect(Collectors.toList());
    }

    public List<ILoggingEvent> search(Level level) {
      return this.list.stream()
          .filter(event -> event.toString().contains(MSG)
              && event.getLevel().equals(level))
          .collect(Collectors.toList());
    }

    public int getSize() {
      return this.list.size();
    }

    public List<ILoggingEvent> getLoggedEvents() {
      return Collections.unmodifiableList(this.list);
    }
  }

}
