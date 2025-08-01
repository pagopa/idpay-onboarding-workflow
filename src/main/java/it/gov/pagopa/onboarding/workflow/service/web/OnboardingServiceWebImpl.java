package it.gov.pagopa.onboarding.workflow.service.web;

import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.InitiativeWebMapper;
import it.gov.pagopa.onboarding.workflow.service.OnboardingServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.Locale;

@Slf4j
@Service
public class OnboardingServiceWebImpl implements OnboardingServiceWeb {

  private final InitiativeWebMapper initiativeWebMapper;
  private final OnboardingServiceImpl onboardingServiceImpl;

  public OnboardingServiceWebImpl(InitiativeWebMapper initiativeWebMapper,
                                  OnboardingServiceImpl onboardingServiceImpl
                                  ){
    this.initiativeWebMapper = initiativeWebMapper;
    this.onboardingServiceImpl = onboardingServiceImpl;
  }

  @Override
  public InitiativeWebDTO getInitiativeWeb(String initiativeId, Locale acceptLanguage){
    InitiativeDTO initiativeDTO = onboardingServiceImpl.getInitiative(initiativeId);
    if(initiativeDTO != null) {
        return initiativeWebMapper.map(initiativeDTO);
    } else {
        return null;
    }
  }





}
