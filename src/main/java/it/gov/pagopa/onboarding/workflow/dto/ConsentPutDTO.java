package it.gov.pagopa.onboarding.workflow.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ConsentPutDTO {

    @NotBlank(message = "The initiativeId is mandatory")
    private String initiativeId;
    private boolean pdndAccept;
    private List<SelfConsentDTO> selfDeclarationList;
    private String userMail;
    private String userMailConfirmation;
    private Boolean confirmedTos;
}
