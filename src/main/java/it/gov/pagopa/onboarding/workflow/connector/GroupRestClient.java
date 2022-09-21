package it.gov.pagopa.onboarding.workflow.connector;

import it.gov.pagopa.onboarding.workflow.dto.initiative.CitizenStatusDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.ResponseBody;

@FeignClient(
    name = "${rest-client.group.serviceCode}",
    url = "${rest-client.group.baseUrl}")
public interface GroupRestClient {

  @GetMapping(
      value = "/idpay/group/initiative/{initiativeId}/citizen/{userId}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  CitizenStatusDTO getCitizenStatus(
      @PathVariable("initiativeId") String initiativeId, @PathVariable("userId") String userId);
}