package it.gov.pagopa.onboarding.workflow.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@JsonInclude(JsonInclude.Include.NON_NULL)
@AllArgsConstructor
@NoArgsConstructor
@Data
@EqualsAndHashCode
public class ErrorDTO {

  @NotBlank
  private String code;
  @NotBlank
  private String message;
  private String details = null;


  public ErrorDTO(int code, String message, String details) {
    this.code = code+"";
    this.message = message;
    this.details = details;
  }
}

