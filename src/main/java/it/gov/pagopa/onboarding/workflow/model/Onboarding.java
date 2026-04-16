package it.gov.pagopa.onboarding.workflow.model;

import lombok.Data;
import lombok.experimental.FieldNameConstants;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.Instant;

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
    return "%s_%s".formatted(userId, initiativeId);
  }

  @Id
  private String id;
  private String userId;
  private String familyId;
  private String initiativeId;

  private String status;

  private String detailKO;

  private String userMail;

  private String channel;

  private Boolean tc;

  private Boolean pdndAccept;

  private Instant tcAcceptTimestamp;

  private Instant criteriaConsensusTimestamp;

  private Instant requestDeactivationDate;

  private Instant invitationDate;
  private Instant demandedDate;

  private Instant onboardingOkDate;

  private Instant onboardingKODate;

  private Instant updateDate;

  private Instant creationDate;
  private Instant suspensionDate;
  private String name;
  private String surname;

  private String detail;

}
