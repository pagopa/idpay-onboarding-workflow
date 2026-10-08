package it.gov.pagopa.onboarding.workflow.dto.web.mapper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import it.gov.pagopa.common.config.JsonConfig;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeGeneralDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import java.time.LocalDate;
import java.util.Locale;
import java.util.Map;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

class GeneralWebMapperTest {

  private final GeneralWebMapper mapper = new GeneralWebMapper();
  private final ObjectMapper objectMapper = new JsonConfig().objectMapper();

  @Test
  void map_shouldNotExposeInternalFields() {
    InitiativeGeneralDTO generalDTO = new InitiativeGeneralDTO();
    generalDTO.setStartDate(LocalDate.of(2026, 1, 1));
    generalDTO.setEndDate(LocalDate.of(2026, 12, 31));
    generalDTO.setDescriptionMap(Map.of("it", "termini"));
    generalDTO.setFamilyUnitComposition("INPS");
    generalDTO.setBeneficiaryType("NF");

    InitiativeGeneralWebDTO result = mapper.map(generalDTO, Locale.ITALIAN);
    JsonNode json = objectMapper.valueToTree(result);

    assertEquals("termini", result.getTermAndCondition());
    assertEquals(LocalDate.of(2026, 1, 1), result.getStartDate());
    assertEquals(LocalDate.of(2026, 12, 31), result.getEndDate());
    assertFalse(json.has("familyUnitComposition"));
    assertFalse(json.has("beneficiaryType"));
    assertEquals("INPS", generalDTO.getFamilyUnitComposition());
  }
}

