package it.gov.pagopa.onboarding.workflow.dto.notification;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class NotificationQueueDTO {
  private String operationType;
  private String userId;
  private String initiativeId;
  private String serviceId;
  private String status;
  private String initiativeName;
}

