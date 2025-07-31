package it.gov.pagopa.onboarding.workflow.service.web;

import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.ConsentPutWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.OnBoardingWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.GeneralWebMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.InitiativeWebMapper;
import it.gov.pagopa.onboarding.workflow.event.producer.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.exception.custom.*;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import it.gov.pagopa.onboarding.workflow.repository.SelfDeclarationRepository;
import it.gov.pagopa.onboarding.workflow.service.common.OnboardingServiceCommonImpl;
import it.gov.pagopa.onboarding.workflow.utils.AuditUtilities;
import it.gov.pagopa.onboarding.workflow.utils.Utilities;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Locale;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.PDND_CONSENT_DENIED;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.*;

@Slf4j
@Service
public class OnboardingServiceWebImpl extends OnboardingServiceCommonImpl implements OnboardingServiceWeb {

  private final InitiativeWebMapper initiativeWebMapper;
  private final GeneralWebMapper generalWebMapper;

  public OnboardingServiceWebImpl(InitiativeWebMapper initiativeWebMapper,
                                  GeneralWebMapper generalWebMapper,
                                  OnboardingRepository onboardingRepository,
                                  AuditUtilities auditUtilities,
                                  Utilities utilities,
                                  AdmissibilityRestConnector admissibilityRestConnector,
                                  SelfDeclarationRepository selfDeclarationRepository,
                                  ConsentMapper consentMapper,
                                  OnboardingProducer onboardingProducer,
                                  InitiativeRestConnector initiativeRestConnector
                                  ){
      super(auditUtilities, utilities, onboardingRepository, admissibilityRestConnector, selfDeclarationRepository, consentMapper, onboardingProducer, initiativeRestConnector);
    this.initiativeWebMapper = initiativeWebMapper;
    this.generalWebMapper = generalWebMapper;
  }

  @Override
  public InitiativeWebDTO getInitiativeWeb(String initiativeId, Locale acceptLanguage){
    InitiativeDTO initiativeDTO = getInitiative(initiativeId);
    InitiativeGeneralWebDTO initiativeGeneralWebDTO = generalWebMapper.map(initiativeDTO.getGeneral(), acceptLanguage);
    return  initiativeWebMapper.map(initiativeDTO, initiativeGeneralWebDTO);
  }

    public void saveConsentWeb(ConsentPutWebDTO consentPutWebDTO, String userId) {
        long startTime = System.currentTimeMillis();

        Onboarding onboarding = findByInitiativeIdAndUserId(consentPutWebDTO.getInitiativeId(), userId);

        if (onboarding != null) {
            if (OnboardingWorkflowConstants.STATUS_IDEMPOTENT.contains(onboarding.getStatus())) {
                return;
            }

            checkStatus(onboarding);

        } else {

            if (!consentPutWebDTO.getUserMail().equals(consentPutWebDTO.getUserMailConfirmation())) {
                throw new EmailNotMatchedException("Email and confirmation email do not match.");
            }

            if (Boolean.FALSE.equals(consentPutWebDTO.getConfirmedTos())) {
                throw new TosNotConfirmedException("Terms and Conditions not accepted.");
            }

            InitiativeDTO initiativeDTO = getInitiative(consentPutWebDTO.getInitiativeId());

            onboarding = new Onboarding(consentPutWebDTO.getInitiativeId(), userId);

            checkDates(initiativeDTO, onboarding);
            checkBudget(initiativeDTO, onboarding);

            if (!initiativeDTO.getBeneficiaryRule().getAutomatedCriteria().isEmpty()
                    && !consentPutWebDTO.isPdndAccept()) {
                performanceLog(startTime, "SAVE_CONSENT", userId, initiativeDTO.getInitiativeId());
                auditUtilities.logOnboardingKOWithReason(userId, initiativeDTO.getInitiativeId(), onboarding.getChannel(),
                        OnboardingWorkflowConstants.ERROR_PDND_AUDIT);
                onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
                onboarding.setDetailKO(PDND_CONSENT_DENIED);
                onboardingRepository.save(onboarding);
                throw new PDNDConsentDeniedException(String.format(ERROR_PDND_MSG, consentPutWebDTO.getInitiativeId()));
            }

            selfDeclaration(initiativeDTO, consentPutWebDTO, userId);

            onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
            onboarding.setPdndAccept(consentPutWebDTO.isPdndAccept());
            LocalDateTime localDateTime = LocalDateTime.now();
            onboarding.setCriteriaConsensusTimestamp(localDateTime);
            onboarding.setTc(consentPutWebDTO.getConfirmedTos());
            onboarding.setPdndAccept(true);
            onboarding.setUpdateDate(localDateTime);
            onboarding.setUserMail(consentPutWebDTO.getUserMail());

            OnBoardingWebDTO onboardingWebDTO = (OnBoardingWebDTO)consentMapper.map(onboarding);
            onboardingWebDTO.setServiceId(initiativeDTO.getAdditionalInfo().getServiceId());

            onboardingProducer.sendSaveConsent(onboardingWebDTO);

            onboardingRepository.save(onboarding);

            performanceLog(startTime, "SAVE_CONSENT", userId, initiativeDTO.getInitiativeId());
        }
    }


}
