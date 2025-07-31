package it.gov.pagopa.onboarding.workflow.service.web;

import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.GeneralWebMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.InitiativeWebMapper;
import it.gov.pagopa.onboarding.workflow.exception.custom.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.Locale;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.*;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.*;

@Slf4j
@Service
public class OnboardingServiceWebImpl implements OnboardingServiceWeb {

  private final InitiativeWebMapper initiativeWebMapper;
  private final GeneralWebMapper generalWebMapper;

  private final InitiativeRestConnector initiativeRestConnector;

  public OnboardingServiceWebImpl(InitiativeWebMapper initiativeWebMapper,
                                  GeneralWebMapper generalWebMapper,
                                  InitiativeRestConnector initiativeRestConnector
                                  ){
    this.initiativeWebMapper = initiativeWebMapper;
    this.generalWebMapper = generalWebMapper;
    this.initiativeRestConnector = initiativeRestConnector;
  }

  @Override
  public InitiativeWebDTO getInitiativeWeb(String initiativeId, Locale acceptLanguage){
    InitiativeDTO initiativeDTO = getInitiative(initiativeId);
    InitiativeGeneralWebDTO initiativeGeneralWebDTO = generalWebMapper.map(initiativeDTO.getGeneral(), acceptLanguage);
    return  initiativeWebMapper.map(initiativeDTO, initiativeGeneralWebDTO);
  }


  private InitiativeDTO getInitiative(String initiativeId) {
      String sanitizedInitiativeId = initiativeId.replace("\n", "").replace("\r", "");
      log.info("[GET_INITIATIVE] Retrieving information for initiative {}", sanitizedInitiativeId);
      InitiativeDTO initiativeDTO = initiativeRestConnector.getInitiativeBeneficiaryView(initiativeId);
      if (initiativeDTO != null) {
          log.info("Initiative DTO: {}", initiativeDTO);
          if (!OnboardingWorkflowConstants.PUBLISHED.equals(initiativeDTO.getStatus())) {
              log.info("[GET_INITIATIVE] Initiative {} is not PUBLISHED! Status: {}", sanitizedInitiativeId,
                      initiativeDTO.getStatus());
              throw new InitiativeInvalidException(INITIATIVE_NOT_PUBLISHED,
                      String.format(ERROR_INITIATIVE_NOT_ACTIVE_MSG, initiativeId));
          }else {
              log.info("[GET_INITIATIVE] Initiative {} is PUBLISHED", sanitizedInitiativeId);
              return initiativeDTO;
          }
      }else {
          log.warn("[GET_INITIATIVE] initiativeDTO is null for id {}", sanitizedInitiativeId);
          return null;
      }

  }



}
