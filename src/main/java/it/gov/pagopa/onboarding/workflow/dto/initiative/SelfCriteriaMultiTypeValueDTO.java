package it.gov.pagopa.onboarding.workflow.dto.initiative;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class SelfCriteriaMultiTypeValueDTO {

  private String description;

  private String subDescription;

  private String value;

}
