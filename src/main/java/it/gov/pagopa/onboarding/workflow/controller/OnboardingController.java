/**
 * NOTE: This class is auto generated by the swagger code generator program (2.4.27).
 * https://github.com/swagger-api/swagger-codegen Do not edit the class manually.
 */
package it.gov.pagopa.onboarding.workflow.controller;

import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.RequiredCriteriaDTO;
import it.gov.pagopa.onboarding.workflow.dto.UnsubscribeBodyDTO;
import javax.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * IdPay - Citizen Onboarding
 */

@RestController
@RequestMapping("/idpay/onboarding")
public interface OnboardingController {

  /**
   * Acceptance of Terms & Conditions
   *
   * @param body
   * @return
   */
  @PutMapping("/citizen/{userId}")
  ResponseEntity<Void> onboardingCitizen(@RequestBody OnboardingPutDTO body,
      @PathVariable("userId") String userId);

  /**
   * Check the initiative prerequisites
   *
   * @param body
   * @return
   */
  @PutMapping("/initiative/{userId}")
  ResponseEntity<RequiredCriteriaDTO> checkPrerequisites(@RequestBody OnboardingPutDTO body,
      @PathVariable("userId") String userId);


  /**
   * Returns the actual onboarding status
   *
   * @param initiativeId
   * @param userId
   * @return
   */
  @GetMapping("/{initiativeId}/{userId}/status")
  ResponseEntity<OnboardingStatusDTO> onboardingStatus(
      @PathVariable("initiativeId") String initiativeId, @PathVariable("userId") String userId);


  /**
   * Save the consents of PDND criteria and Self declaration list
   *
   * @param body
   * @return
   */
  @PutMapping("/consent/{userId}")
  ResponseEntity<Void> saveConsent(@RequestBody ConsentPutDTO body,
      @PathVariable("userId") String userId);

  /**
   * Deactivation onboarding
   *
   * @param body
   * @return
   */
  @DeleteMapping("/disable")
  ResponseEntity<Void> disableOnboarding(
      @Valid @RequestBody UnsubscribeBodyDTO body);


  /**
   * rollback onboarding
   *
   * @param initiativeId
   * @param userId
   * @return
   */
  @PutMapping("/rollback/{initiativeId}/{userId}")
  ResponseEntity<Void> rollback(
      @PathVariable("initiativeId") String initiativeId, @PathVariable("userId") String userId);

}
