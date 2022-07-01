package it.gov.pagopa.onboarding.workflow.dto;

import java.util.List;
import javax.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsentPutDTO {

  @NotBlank(message="Field initiativeId cannot be blank!")
  String initiativeId = null;

  boolean pdndAccept;

  List<SelfConsentDTO> selfDeclarationList = null;

}

