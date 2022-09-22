package it.gov.pagopa.onboarding.workflow.event.producer;

import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.stereotype.Component;

@Component
public class OutcomeProducer {

  private final String binder;

  private final StreamBridge streamBridge;

  public OutcomeProducer(StreamBridge streamBridge,
      @Value("${spring.cloud.stream.bindings.onboarding-out-0.binder}") String binder) {
    this.streamBridge = streamBridge;
    this.binder = binder;
  }

  public void sendOutcome(EvaluationDTO evaluationDTO) {
    streamBridge.send("onboarding-out-0", binder, evaluationDTO);
  }

}
