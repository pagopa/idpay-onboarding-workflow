package it.gov.pagopa.onboarding.workflow.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.*;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ConsentPutUnifiedDTO {

    @NotBlank(message = "The initiativeId is mandatory")
    private String initiativeId;

    private boolean pdndAccept;

    private List<SelfConsentDTO> selfDeclarationList;


    private String userMail;
    private String userMailConfirmation;
    private Boolean confirmedTos;

    @NotBlank(message = "Channel is mandatory")
    private String channel;

    public boolean isWebChannel() {
        return "WEB".equalsIgnoreCase(channel);
    }

    public boolean isIoChannel() {
        return "IO".equalsIgnoreCase(channel);
    }
}
