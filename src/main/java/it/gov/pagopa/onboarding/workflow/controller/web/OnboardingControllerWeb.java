
package it.gov.pagopa.onboarding.workflow.controller.web;


import it.gov.pagopa.onboarding.workflow.dto.ConsentPutUnifiedDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.enums.ChannelType;
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
   * @param consentPutUnifiedDTO
   * @param userId
   * @return
   */
  @PutMapping("/{userId}")
  ResponseEntity<Void> saveConsentUnified(
          @RequestBody ConsentPutUnifiedDTO consentPutUnifiedDTO,
          @RequestHeader("X-Channel") ChannelType channel,
          @PathVariable("userId") String userId);
}
