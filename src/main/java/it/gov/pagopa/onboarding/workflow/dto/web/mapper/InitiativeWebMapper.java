package it.gov.pagopa.onboarding.workflow.dto.web.mapper;

import it.gov.pagopa.onboarding.workflow.dto.initiative.*;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import org.jspecify.annotations.Nullable;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class InitiativeWebMapper {

  public InitiativeWebDTO map(InitiativeDTO initiativeDTO, InitiativeGeneralWebDTO initiativeGeneralWebDTO) {

    return InitiativeWebDTO.builder()
        .additionalInfo(initiativeDTO.getAdditionalInfo())
        .beneficiaryRule(updateBeneficiaryRuleWeb(initiativeDTO))
        .generalWeb(initiativeGeneralWebDTO)
        .build();

  }

  private static @Nullable InitiativeBeneficiaryRuleDTO updateBeneficiaryRuleWeb(InitiativeDTO initiativeDTO) {
    InitiativeBeneficiaryRuleDTO beneficiaryRule = initiativeDTO.getBeneficiaryRule();

    if (beneficiaryRule != null && beneficiaryRule.getSelfDeclarationCriteria() != null) {
      List<SelfDeclarationItemsDTO> mappedCriteria = new ArrayList<>(beneficiaryRule.getSelfDeclarationCriteria().size());

      for (SelfDeclarationItemsDTO criteria : beneficiaryRule.getSelfDeclarationCriteria()) {
        if (criteria instanceof SelfCriteriaMultiTypeDTO multiTypeCriteria) {
          List<SelfCriteriaMultiTypeValueDTO> valuesWeb = new ArrayList<>(multiTypeCriteria.getValue().size());
          multiTypeCriteria.getValue()
                  .forEach(value -> {
                    SelfCriteriaMultiTypeValueDTO valueWeb = SelfCriteriaMultiTypeValueDTO.builder()
                            .description(value.getDescription())
                            .subDescription(value.getSubDescription())
                            .value(value.getValue())
                            .build();
                    valuesWeb.add(valueWeb);
                  });
          multiTypeCriteria.setValue(valuesWeb);

          mappedCriteria.add(multiTypeCriteria);
        } else {
          mappedCriteria.add(criteria);
        }
      }

      beneficiaryRule.setSelfDeclarationCriteria(mappedCriteria);
    }
    return beneficiaryRule;
  }

}
