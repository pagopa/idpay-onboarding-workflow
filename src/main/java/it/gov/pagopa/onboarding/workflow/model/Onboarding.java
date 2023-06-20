package it.gov.pagopa.onboarding.workflow.model;

import java.time.LocalDateTime;
import lombok.Data;
import lombok.experimental.FieldNameConstants;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Data
@FieldNameConstants
@Document(collection = "onboarding_citizen")
public class Onboarding {

  public Onboarding(String initiativeId, String userId) {

    this.initiativeId = initiativeId;
    this.userId = userId;

    this.id = buildId(initiativeId, userId);
  }

  public static String buildId(String initiativeId, String userId) {
    return "%s_%s".formatted(initiativeId, userId);
  }

  @Id
  private String id;
  private String userId;
  private String familyId;
  private String initiativeId;

  private String status;

  private String detailKO;

  private String channel;

  private Boolean tc;

  private Boolean pdndAccept;

  private LocalDateTime tcAcceptTimestamp;

  private LocalDateTime criteriaConsensusTimestamp;

  private LocalDateTime requestDeactivationDate;

  private LocalDateTime invitationDate;
  private LocalDateTime demandedDate;

  private LocalDateTime onboardingOkDate;

  private LocalDateTime onboardingKODate;

  private LocalDateTime updateDate;

  private LocalDateTime creationDate;
  private LocalDateTime suspensionDate;

}
