package it.gov.pagopa.onboarding.workflow.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CheckPastOnboardingsSeriesResDTO {
    private List<CheckPastOnboardingsSeriesResElemDTO> pastOnboardings;
}
