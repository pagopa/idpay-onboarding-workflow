package it.gov.pagopa.onboarding.workflow.dto.web.mapper;

import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeGeneralDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import org.springframework.stereotype.Service;

import java.util.Locale;

@Service
public class GeneralWebMapper {

  public InitiativeGeneralWebDTO map(InitiativeGeneralDTO generalDTO, Locale acceptLanguage) {


    return InitiativeGeneralWebDTO.builder()
            .startDate(generalDTO.getStartDate())
            .endDate(generalDTO.getEndDate())
            .termAndCondition(generalDTO.getDescriptionMap().get(acceptLanguage.getLanguage()))
            .build();
  }

}
