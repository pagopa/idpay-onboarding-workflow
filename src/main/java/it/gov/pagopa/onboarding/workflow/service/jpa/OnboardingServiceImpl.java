package it.gov.pagopa.onboarding.workflow.service.jpa;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingStatusDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Date;

@Service
public class OnboardingServiceImpl implements OnboardingService {

  @Autowired
  private OnboardingRepository onboardingRepository;

  @Override
  public Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId) {
    return onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId);
  }

  @Override
  public OnboardingStatusDTO getOnboardingStatus(String initiativeId, String userId) {
    OnboardingStatusDTO onboardingStatusDTO = null;
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId);
    if (onboarding != null) {
      onboardingStatusDTO = new OnboardingStatusDTO(onboarding.getStatus());
    }
    //System.out.println(onboardingStatusDTO);
    return onboardingStatusDTO;
  }

  @Override
  public ResponseEntity<?> putTcConsent(String initiativeId, String userId) {
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId, userId);
    if(onboarding == null){
      Onboarding newOnboarding = new Onboarding();
      newOnboarding.setUserId(userId);
      newOnboarding.setInitiativeId(initiativeId);
      newOnboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
      newOnboarding.setTcAcceptTimestamp(new Date());
      newOnboarding.setTc(true);
      onboardingRepository.save(newOnboarding);


    }else{
      if (!onboarding.isTc()) { //se non è flaggato tc
        onboarding.setTc(true);
        onboarding.setTcAcceptTimestamp(new Date());
        onboarding.setStatus(OnboardingWorkflowConstants.ACCEPTED_TC);
        onboardingRepository.save(onboarding);
      }

    }

    return new ResponseEntity<>(HttpStatus.NO_CONTENT);

  }

  @Override
  public void setOnEvaluation(Onboarding onboarding) {
    onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
    onboardingRepository.save(onboarding);
  }
}
