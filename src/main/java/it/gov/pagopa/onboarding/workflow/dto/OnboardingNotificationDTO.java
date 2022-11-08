package it.gov.pagopa.onboarding.workflow.dto;

import java.time.LocalDateTime;
import java.util.Map;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class OnboardingNotificationDTO {

  private String operationType;
  private String userId;
  private String initiativeId;
  private String serviceId;
  private String initiativeName;

}
