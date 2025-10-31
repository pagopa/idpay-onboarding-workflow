package it.gov.pagopa.onboarding.workflow.dto.initiative;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Future;
import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.*;
import org.springframework.validation.annotation.Validated;

import java.time.LocalDate;
import java.util.Map;

/**
 * InitiativeDTO
 */
@Validated
@Data
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class InitiativeIssuerDTO {

  @JsonProperty("initiativeId")
  private String initiativeId;

  @JsonProperty("initiativeName")
  private String initiativeName;

  @JsonProperty("organizationName")
  private String organizationName;

  @JsonProperty("descriptionMap")
  @Valid
  @NotEmpty(groups = ValidationApiEnabledGroup.class)
  private Map<String, String> descriptionMap;

  @JsonProperty("startDate")
  @NotNull(groups = ValidationApiEnabledGroup.class)
  private LocalDate startDate;

  @JsonProperty("endDate")
  @NotNull(groups = ValidationApiEnabledGroup.class)
  private LocalDate endDate;

  @JsonProperty("rankingEnabled")
  private Boolean rankingEnabled;

  @JsonProperty("rankingStartDate")
  @FutureOrPresent(groups = ValidationApiEnabledGroup.class)
  private LocalDate rankingStartDate;

  @JsonProperty("rankingEndDate")
  @Future(groups = ValidationApiEnabledGroup.class)
  private LocalDate rankingEndDate;

  @JsonProperty("beneficiaryKnown")
  private Boolean beneficiaryKnown;

  @JsonProperty("status")
  private String status;

  @JsonProperty("tcLink")
  private String tcLink;

  @JsonProperty("privacyLink")
  private String privacyLink;

  @JsonProperty("logoURL")
  private String logoURL;

}
