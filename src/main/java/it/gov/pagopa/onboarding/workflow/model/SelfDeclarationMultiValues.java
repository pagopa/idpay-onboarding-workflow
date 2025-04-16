package it.gov.pagopa.onboarding.workflow.model;

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
    private List<String> values;
    private String code;

}
