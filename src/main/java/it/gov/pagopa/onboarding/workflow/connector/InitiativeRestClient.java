package it.gov.pagopa.onboarding.workflow.connector;

import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeIssuerDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;

@FeignClient(
        name = "${rest-client.initiative.serviceCode}",
        url = "${rest-client.initiative.baseUrl}")
public interface InitiativeRestClient {

  @GetMapping(
          value = "/idpay/initiative/{initiativeId}/beneficiary/view",
          produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  InitiativeDTO getInitiativeBeneficiaryView(
          @PathVariable("initiativeId") String initiativeId);

  @GetMapping(
          value = "/idpay/initiatives",
          produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  List<InitiativeIssuerDTO> getInitiativeIssuerList();

}