package it.gov.pagopa.onboarding.workflow.model;

import java.time.LocalDateTime;
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

  private String datailKO;

  private String channel;

  private Boolean tc;

  private Boolean pdndAccept;

  private LocalDateTime tcAcceptTimestamp;

  private LocalDateTime criteriaConsensusTimestamp;

  private LocalDateTime requestDeactivationDate;

  private LocalDateTime invitationDate;

  private LocalDateTime onboardingOkDate;

  private LocalDateTime onboardingKODate;

  private LocalDateTime updateDate;

  private LocalDateTime creationDate;

}
