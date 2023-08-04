package it.gov.pagopa.onboarding.workflow.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class QueueCommandOperationDTO {

    String operationType;
    String entityId;
    LocalDateTime operationTime;

}
