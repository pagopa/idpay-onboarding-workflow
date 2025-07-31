package it.gov.pagopa.onboarding.workflow.dto.web.mapper;

import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeGeneralDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import org.springframework.stereotype.Service;

import java.util.Locale;

@Service
public class GeneralWebMapper {

  public InitiativeGeneralWebDTO map(InitiativeGeneralDTO generalDTO, Locale acceptLanguage) {


    return InitiativeGeneralWebDTO.builder()
            .budget(generalDTO.getBudget())
            .beneficiaryType(generalDTO.getBeneficiaryType())
            .familyUnitComposition(generalDTO.getFamilyUnitComposition())
            .beneficiaryKnown(generalDTO.getBeneficiaryKnown())
            .beneficiaryBudget(generalDTO.getBeneficiaryBudget())
            .startDate(generalDTO.getStartDate())
            .endDate(generalDTO.getEndDate())
            .rankingStartDate(generalDTO.getRankingStartDate())
            .rankingEndDate(generalDTO.getRankingEndDate())
            .rankingEnabled(generalDTO.getRankingEnabled())
            .termAndCondition(generalDTO.getDescriptionMap().get(acceptLanguage.getLanguage()))
            .build();
  }

}
