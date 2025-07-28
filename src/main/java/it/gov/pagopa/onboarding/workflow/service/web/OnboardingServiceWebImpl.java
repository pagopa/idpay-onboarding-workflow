package it.gov.pagopa.onboarding.workflow.service.web;

import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.InitiativeWebMapper;
import it.gov.pagopa.onboarding.workflow.exception.custom.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.*;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.*;

@Slf4j
@Service
public class OnboardingServiceWebImpl implements OnboardingServiceWeb {

  private final InitiativeWebMapper initiativeWebMapper;

  private final InitiativeRestConnector initiativeRestConnector;

  public OnboardingServiceWebImpl(InitiativeWebMapper initiativeWebMapper,
                                  InitiativeRestConnector initiativeRestConnector
                                  ){
    this.initiativeWebMapper = initiativeWebMapper;
    this.initiativeRestConnector = initiativeRestConnector;
  }

  @Override
  public InitiativeWebDTO getInitiativeWeb(String initiativeId){
    InitiativeDTO initiativeDTO = getInitiative(initiativeId);
    return  initiativeWebMapper.map(initiativeDTO);
  }


  private InitiativeDTO getInitiative(String initiativeId) {
      log.info("[GET_INITIATIVE] Retrieving information for initiative {}", initiativeId);
      InitiativeDTO initiativeDTO = initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId);
      log.info(initiativeDTO.toString());
      if (!initiativeDTO.getStatus().equals(OnboardingWorkflowConstants.PUBLISHED)) {
        log.info("[GET_INITIATIVE] Initiative {} is not active PUBLISHED! Status: {}", initiativeId,
            initiativeDTO.getStatus());
        throw new InitiativeInvalidException(INITIATIVE_NOT_PUBLISHED,
                String.format(ERROR_INITIATIVE_NOT_ACTIVE_MSG, initiativeId));
      }
      log.info("[GET_INITIATIVE] Initiative {} is PUBLISHED", initiativeId);
      return initiativeDTO;
  }

}
