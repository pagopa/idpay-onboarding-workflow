package it.gov.pagopa.onboarding.workflow.dto.web.mapper;

import it.gov.pagopa.onboarding.workflow.dto.initiative.*;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class InitiativeWebMapperTest {

    private final InitiativeWebMapper initiativeWebMapper = new InitiativeWebMapper();

    @Test
    void map_shouldReturnOnlyDescriptionSubDescriptionAndValueForMultiTypeCriteria() {
        InitiativeDTO initiativeDTO = new InitiativeDTO();
        InitiativeBeneficiaryRuleDTO beneficiaryRule = new InitiativeBeneficiaryRuleDTO();

        SelfCriteriaMultiTypeValueDTO option = SelfCriteriaMultiTypeValueDTO.builder()
                .description("ISEE sopra 25k")
                .subDescription("opzione")
                .value("1")
                .verify(true)
                .thresholdCode("ISEE")
                .beneficiaryBudgetCentsMin(100L)
                .beneficiaryBudgetCentsMax(100L)
                .blockingVerify(true)
                .build();

        SelfCriteriaMultiTypeDTO multiTypeCriteria = new SelfCriteriaMultiTypeDTO(
                "multi_consent",
                "descrizione multi",
                "sub descrizione multi",
                List.of(option),
                "MULTI_CODE"
        );

        SelfCriteriaBoolDTO boolCriteria = new SelfCriteriaBoolDTO(
                "boolean",
                "descrizione bool",
                "sub descrizione bool",
                Boolean.TRUE,
                "BOOL_CODE"
        );

        beneficiaryRule.setSelfDeclarationCriteria(new ArrayList<>(List.of(multiTypeCriteria, boolCriteria)));
        initiativeDTO.setBeneficiaryRule(beneficiaryRule);

        InitiativeWebDTO result = initiativeWebMapper.map(initiativeDTO, null);

        result.getBeneficiaryRule().getSelfDeclarationCriteria().forEach(criteria -> {
            if (criteria instanceof SelfCriteriaMultiTypeDTO multiType) {
                assertNotNull(multiType.getDescription());
                assertNotNull(multiType.getSubDescription());
                List<SelfCriteriaMultiTypeValueDTO> value = multiType.getValue();
                assertNotNull(value);
                assertNotNull(value.getFirst());
                assertNull(value.getFirst().getVerify());
                assertNull(value.getFirst().getThresholdCode());
                assertNull(value.getFirst().getBeneficiaryBudgetCentsMin());
                assertNull(value.getFirst().getBeneficiaryBudgetCentsMax());
                assertNull(value.getFirst().getBlockingVerify());
            } else if (criteria instanceof SelfCriteriaBoolDTO bool) {
                assertNotNull(bool.getDescription());
                assertNotNull(bool.getSubDescription());
                assertNotNull(bool.getValue());
            }
        });
    }
}



