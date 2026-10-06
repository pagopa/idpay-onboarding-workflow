package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
@AllArgsConstructor
public class QueueCommandOperationDTO {

    String operationType;
    String entityId;
    LocalDateTime operationTime;

}
