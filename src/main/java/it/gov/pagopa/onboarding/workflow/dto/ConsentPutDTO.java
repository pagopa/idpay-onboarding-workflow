package it.gov.pagopa.onboarding.workflow.dto;

import java.util.List;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsentPutDTO {

  @NotBlank(message="The field is mandatory!")
  String initiativeId;

  boolean pdndAccept;

  List<SelfConsentDTO> selfDeclarationList;

}

