package it.gov.pagopa.onboarding.workflow.dto;

import java.time.LocalDateTime;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class EvaluationDTO {

  String userId;
  String initiativeId;
  String status;
  LocalDateTime admissibilityCheckDate;
  List<String> onboardingRejectionReasons;
}