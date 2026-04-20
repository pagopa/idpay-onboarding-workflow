package it.gov.pagopa.onboarding.workflow.model;

import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaMultiTypeValueDTO;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class SelfDeclarationMultiValues {

    private String type;
    private String description;
    private List<SelfCriteriaMultiTypeValueDTO> values;
    private String code;

}
