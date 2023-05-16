package it.gov.pagopa.onboarding.workflow.connector.admissibility;

import it.gov.pagopa.onboarding.workflow.dto.admissibility.InitiativeStatusDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.ResponseBody;

@FeignClient(
    name = "${rest-client.admissibility.serviceCodeAdm}",
    url = "${rest-client.admissibility.baseUrl}")
public interface AdmissibilityRestClient {

  @GetMapping(
      value = "/idpay/admissibility/initiative/{initiativeId}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  InitiativeStatusDTO getInitiativeStatus(
      @PathVariable("initiativeId") String initiativeId);

}
