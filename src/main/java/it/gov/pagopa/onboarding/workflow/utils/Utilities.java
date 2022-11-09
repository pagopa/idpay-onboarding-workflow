package it.gov.pagopa.onboarding.workflow.utils;

import java.util.logging.Logger;
import lombok.Data;
import org.springframework.stereotype.Component;

@Component
@Data
public class Utilities {
  private static final String PRODUCT = "product=IDPAY|";
  private static final String EVENT = "event=Onboarding|";


  private String buildLog(String eventLog){
    eventLog = PRODUCT + EVENT + eventLog;
    return eventLog;
  }

  public void logTC(){
    Logger logger = Logger.getLogger("AUDIT");
    String testLog = this.buildLog("Terms and conditions accepted by the citizen ");
    logger.info(testLog);
  }

  public void logPDND(){
    Logger logger = Logger.getLogger("AUDIT");
    String testLog = this.buildLog("Prerequisites required passed ");
    logger.info(testLog);
  }

  public void logOnboardingOk(){
    Logger logger = Logger.getLogger("AUDIT");
    String testLog = this.buildLog("Onboarding of the citizen complete");
    logger.info(testLog);
  }



}