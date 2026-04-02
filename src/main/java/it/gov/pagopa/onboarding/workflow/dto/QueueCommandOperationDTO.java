package it.gov.pagopa.onboarding.workflow.dto;

import lombok.Builder;
import lombok.Data;

import java.time.Instant;

@Data
@Builder
public class QueueCommandOperationDTO {

    String operationType;
    String entityId;
    Instant operationTime;

}
