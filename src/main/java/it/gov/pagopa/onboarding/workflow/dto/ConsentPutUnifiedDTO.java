package it.gov.pagopa.onboarding.workflow.dto;

import it.gov.pagopa.onboarding.workflow.enums.ChannelType;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
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

    @NotNull(message = "Channel is mandatory")
    private ChannelType channel;

    public boolean isWebChannel() {
        return ChannelType.WEB == channel;
    }

    public boolean isIoChannel() {
        return ChannelType.APP_IO == channel;
    }
}
