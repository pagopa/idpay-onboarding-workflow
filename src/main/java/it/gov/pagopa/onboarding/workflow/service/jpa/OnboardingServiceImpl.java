package it.gov.pagopa.onboarding.workflow.service.jpa;

import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

@Service
public class OnboardingServiceImpl implements OnboardingService {

  @Autowired
  private OnboardingRepository onboardingRepository;

  @Override
  public OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId) {
    OnboardingStatusDTO onboardingStatusDTO = null;
    Mono<Onboarding> onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId);
    if (onboarding != null) {
      //onboardingStatusDTO = new OnboardingStatusDTO(onboarding.getStatus());
      onboarding.subscribe(System.out::println);
    }
    //System.out.println(onboardingStatusDTO);
    return onboardingStatusDTO;
  }

  @Override
  public ResponseEntity<Void> putTcConsent(String initiativeId, String userId) {
    /*Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId);
    if (onboarding != null) {
      onboarding.setTc(true);
      onboardingRepository.save(onboarding);
    }*/
    return null;
  }
}
