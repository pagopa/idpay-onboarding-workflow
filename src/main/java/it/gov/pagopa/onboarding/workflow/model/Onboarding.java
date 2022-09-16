package it.gov.pagopa.onboarding.workflow.model;

import java.time.LocalDateTime;
import java.util.Map;
import lombok.Data;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.CompoundIndex;
import org.springframework.data.mongodb.core.mapping.Document;

@Data
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

  private boolean tc;

  private Boolean pdndAccept;

  private Map<String, Boolean> selfDeclarationList;

  private LocalDateTime tcAcceptTimestamp;

  private LocalDateTime criteriaConsensusTimestamp;

  private LocalDateTime requestDeactivationDate;

}
