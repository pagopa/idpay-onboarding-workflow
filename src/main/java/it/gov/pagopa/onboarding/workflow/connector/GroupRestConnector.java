package it.gov.pagopa.onboarding.workflow.connector;

import it.gov.pagopa.onboarding.workflow.dto.initiative.CitizenStatusDTO;
import org.springframework.web.bind.annotation.PathVariable;

public interface GroupRestConnector {

  CitizenStatusDTO getCitizenStatus(@PathVariable("initiativeId") String initiativeId,
      @PathVariable("userId") String userId);
}
