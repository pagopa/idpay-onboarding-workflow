package it.gov.pagopa.onboarding.workflow.model;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Document(collection = "self_declaration_text")
@Data
@NoArgsConstructor
@SuperBuilder
public class SelfDeclarationText {

    public static String buildId(String initiativeId, String userId) {
        return "%s_%s".formatted(userId, initiativeId);
    }

    @Id private String id;
    private String initiativeId;
    private String userId;
    private List<SelfDeclarationTextValues> selfDeclarationTextValues;
}
