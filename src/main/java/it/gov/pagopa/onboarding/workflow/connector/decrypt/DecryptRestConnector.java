package it.gov.pagopa.onboarding.workflow.connector.decrypt;

import it.gov.pagopa.onboarding.workflow.dto.DecryptCfDTO;
import org.springframework.stereotype.Service;

@Service
public interface DecryptRestConnector {

  DecryptCfDTO getPiiByToken(String token);
}
