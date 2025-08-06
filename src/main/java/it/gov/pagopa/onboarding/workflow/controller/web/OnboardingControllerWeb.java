
package it.gov.pagopa.onboarding.workflow.controller.web;


import it.gov.pagopa.onboarding.workflow.dto.web.ConsentPutWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Locale;


/**
 * IdPay - Citizen Onboarding - Web
 */

@RestController
@RequestMapping("/idpay/onboarding/web")
public interface OnboardingControllerWeb {


  /**
   * Returns the initiative details
   *
   * @param initiativeId
   * @RequestHeader acceptLanguage
   * @return
   */
  @GetMapping("/{initiativeId}/detail")
  ResponseEntity<InitiativeWebDTO> getInitiativeWeb(
          @PathVariable("initiativeId") String initiativeId,
          @RequestHeader(value = "Accept-Language", defaultValue = "it_IT") Locale acceptLanguage);


  /**
   * Save the consents of PDND criteria and Self declaration list
   *
   * @param consentPutWebDTO
   * @param userId
   * @return
   */
  @PutMapping("/{userId}")
  ResponseEntity<Void> saveConsentWeb(
          @RequestBody ConsentPutWebDTO consentPutWebDTO,
          @PathVariable("userId") String userId);
}
