package it.gov.pagopa.onboarding.workflow.model;

import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfDeclarationItemsDTO;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Data;
import lombok.experimental.FieldNameConstants;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.CompoundIndex;
import org.springframework.data.mongodb.core.mapping.Document;

@Data
@FieldNameConstants
@Document(collection = "onboarding_citizen")
@CompoundIndex(name = "onboarding_unique_idx", def = "{'userId': 1, 'initiativeId': 1}", unique = true)
public class Onboarding {

  public Onboarding(String initiativeId, String userId) {

    this.initiativeId = initiativeId;
    this.userId = userId;

  }

  @Id
  private String id;

  private String userId;

  private String initiativeId;

  private String status;

  private Boolean pdndCheck;

  private Boolean autocertificationCheck;

  private Boolean tc;

  private Boolean pdndAccept;

  private List<SelfDeclarationItemsDTO> selfDeclarationList;

  private LocalDateTime tcAcceptTimestamp;

  private LocalDateTime criteriaConsensusTimestamp;

  private LocalDateTime invitationTimestamp;

  private LocalDateTime requestDeactivationDate;
  private LocalDateTime invitationDate;

  private LocalDateTime onboardingOkDate;

  private LocalDateTime updateDate;
  private LocalDateTime creationDate;

}
