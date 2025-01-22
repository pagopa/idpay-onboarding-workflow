package it.gov.pagopa.onboarding.workflow.dto;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "_type",
    visible = true)
@JsonSubTypes({
    @JsonSubTypes.Type(value = SelfConsentTextDTO.class, name = "text"),
    @JsonSubTypes.Type(value = SelfConsentMultiDTO.class, name = "multi"),
    @JsonSubTypes.Type(value = SelfConsentBoolDTO.class, name = "boolean")
})
public interface SelfConsentDTO {

}

