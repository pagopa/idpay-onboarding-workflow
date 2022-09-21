package it.gov.pagopa.onboarding.workflow.event.producer;

import it.gov.pagopa.onboarding.workflow.dto.EvaluationDTO;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.stereotype.Component;

@Component
public class OutcomeProducer {

  @Value("${spring.cloud.stream.bindings.outcomeProducer-out-0.binder}")
  private String binder;

  private final StreamBridge streamBridge;

  public OutcomeProducer(StreamBridge streamBridge){
    this.streamBridge = streamBridge;
  }

  public void sendOutcome(EvaluationDTO evaluationDTO){
    streamBridge.send("outcomeProducer-out-0", binder, evaluationDTO);
  }

}
