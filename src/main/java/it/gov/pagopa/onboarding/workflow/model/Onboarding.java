package it.gov.pagopa.onboarding.workflow.model;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Data
@NoArgsConstructor
@Document(collection = "onboarding_citizen")
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

}
