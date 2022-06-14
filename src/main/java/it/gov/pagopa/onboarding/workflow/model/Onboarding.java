package it.gov.pagopa.onboarding.workflow.model;

import java.util.Date;
import java.util.List;
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

  private boolean pdndAccept;

  private List<Boolean> selfDeclarationList;

  private Date tcAcceptTimestamp;

  private Date criteriaConsensusTimestamp;


//  public Onboarding(String initiativeId, String userId, boolean tc, String status, Date criteriaConsensusTimestamp) {
//    this.userId = userId;
//    this.initiativeId = initiativeId;
//    this.status = status;
//    this.tc = tc;
//    this.criteriaConsensusTimestamp = criteriaConsensusTimestamp;
//  }
}
