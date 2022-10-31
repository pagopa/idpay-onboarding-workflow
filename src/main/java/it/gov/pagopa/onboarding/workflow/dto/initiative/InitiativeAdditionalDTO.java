package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import lombok.Data;

@Data
public class InitiativeAdditionalDTO   {

  @JsonProperty("serviceIO")
  private Boolean serviceIO;

  @JsonProperty("serviceId")
  private String serviceId;

  @JsonProperty("serviceName")
  private String serviceName;

  @JsonProperty("serviceScope")
  private String serviceScope;

  @JsonProperty("description")
  private String description;

  @JsonProperty("privacyLink")
  private String privacyLink;

  @JsonProperty("tcLink")
  private String tcLink;

  @JsonProperty("channels")
  private List<ChannelDTO> channels;
}
