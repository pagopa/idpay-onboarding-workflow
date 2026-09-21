package it.gov.pagopa.onboarding.workflow.dto;

import com.fasterxml.jackson.databind.ObjectMapper;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaInformativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfDeclarationItemsDTO;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SelfDeclarationItemsDTOTest {

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Test
    void testDeserializeSelfCriteriaInformative() throws Exception {
        String json = """
            {
              "_type": "informative",
              "code": "ADE",
              "description": "Canone TV",
              "organization": "Agenzia delle Entrate",
              "value": "Descrizione estesa del requisito"
            }
            """;

        SelfDeclarationItemsDTO dto = objectMapper.readValue(json, SelfDeclarationItemsDTO.class);

        assertNotNull(dto);
        assertInstanceOf(SelfCriteriaInformativeDTO.class, dto);

        SelfCriteriaInformativeDTO informative = (SelfCriteriaInformativeDTO) dto;
        assertEquals("informative", informative.getType());
        assertEquals("ADE", informative.getCode());
        assertEquals("Canone TV", informative.getDescription());
        assertEquals("Agenzia delle Entrate", informative.getOrganization());
        assertEquals("Descrizione estesa del requisito", informative.getValue());
    }

    @Test
    void testSerializeSelfCriteriaInformative() throws Exception {
        SelfCriteriaInformativeDTO dto = SelfCriteriaInformativeDTO.builder()
                .type("informative")
                .code("ANPR")
                .description("Famiglia anagrafica")
                .organization("ANPR")
                .value("Dettaglio del requisito ANPR")
                .build();

        String jsonResult = objectMapper.writeValueAsString(dto);

        assertTrue(jsonResult.contains("\"_type\":\"informative\""));
        assertTrue(jsonResult.contains("\"code\":\"ANPR\""));
        assertTrue(jsonResult.contains("\"organization\":\"ANPR\""));
    }
}
