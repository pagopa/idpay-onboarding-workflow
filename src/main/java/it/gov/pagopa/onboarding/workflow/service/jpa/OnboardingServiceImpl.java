package it.gov.pagopa.onboarding.workflow.service.jpa;

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
  public ResponseEntity<Void> putTcConsent(String initiativeId, String userId) {
    Onboarding onboarding = onboardingRepository.findByInitiativeIdAndUserId(initiativeId,userId);
    if(onboarding!=null){
      if(!onboarding.isTc()) { //se non è fleggato tc
        onboarding.setTc(true);
        Date dateCurrent = new Date(System.currentTimeMillis());
        onboarding.setTcAcceptTimestamp(dateCurrent);
        onboarding.setStatus("ACCEPTED_TC");
        onboardingRepository.save(onboarding);
      }
      return new ResponseEntity(HttpStatus.OK);
    }
    return null;
  }
















}
