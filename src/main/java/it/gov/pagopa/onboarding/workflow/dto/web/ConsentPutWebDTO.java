package it.gov.pagopa.onboarding.workflow.dto.web;

import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ConsentPutWebDTO extends ConsentPutDTO {

    @NotBlank(message="The field is mandatory")
    private String userMail;

    @NotBlank(message="The field is mandatory")
    private String userMailConfirmation;

    @NotNull
    private Boolean confirmedTos;
}
