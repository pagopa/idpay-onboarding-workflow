package it.gov.pagopa.onboarding.workflow.connector.decrypt;

import it.gov.pagopa.onboarding.workflow.dto.DecryptCfDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.ResponseBody;

@FeignClient(name = "${rest-client.decrypt.cf}", url = "${rest-client.decrypt.baseUrl}")
public interface DecryptRest {

  @GetMapping(value = "/tokens/{token}/pii", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  DecryptCfDTO getPiiByToken(@PathVariable("token") String token,
                             @RequestHeader("x-api-key") String apikey);
}
