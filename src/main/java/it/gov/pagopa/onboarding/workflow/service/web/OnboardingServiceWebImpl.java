package it.gov.pagopa.onboarding.workflow.service.web;

import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ConsentPutUnifiedDTO;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeGeneralWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.InitiativeWebDTO;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.GeneralWebMapper;
import it.gov.pagopa.onboarding.workflow.dto.web.mapper.InitiativeWebMapper;
import it.gov.pagopa.onboarding.workflow.enums.ChannelType;
import it.gov.pagopa.onboarding.workflow.event.producer.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.exception.custom.EmailNotMatchedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.PDNDConsentDeniedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.TosNotConfirmedException;
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
                                  OnboardingRepository onboardingRepository,
                                  AuditUtilities auditUtilities,
                                  Utilities utilities,
                                  AdmissibilityRestConnector admissibilityRestConnector,
                                  SelfDeclarationRepository selfDeclarationRepository,
                                  ConsentMapper consentMapper,
                                  OnboardingProducer onboardingProducer,
                                  InitiativeRestConnector initiativeRestConnector,
                                  GeneralWebMapper generalWebMapper
                                  ) {
      super(auditUtilities, utilities, onboardingRepository, admissibilityRestConnector, selfDeclarationRepository, consentMapper, onboardingProducer, initiativeRestConnector);
      this.initiativeWebMapper = initiativeWebMapper;
      this.generalWebMapper = generalWebMapper;
  }

  @Override
  public InitiativeWebDTO getInitiativeWeb(String initiativeId, Locale acceptLanguage){
    InitiativeDTO initiativeDTO = getInitiative(initiativeId);
    if(initiativeDTO != null) {
    InitiativeGeneralWebDTO initiativeGeneralWebDTO = generalWebMapper.map(initiativeDTO.getGeneral(), acceptLanguage);

        return initiativeWebMapper.map(initiativeDTO, initiativeGeneralWebDTO);
    } else {
        return null;
    }
  }

    @Override
    public void saveConsentUnified(ConsentPutUnifiedDTO consentPutUnifiedDTO, String userId) {
        long startTime = System.currentTimeMillis();

        Onboarding onboarding = findOnboardingByInitiativeIdAndUserId(consentPutUnifiedDTO.getInitiativeId(), userId);

        if (onboarding != null) {
            handleExistingOnboarding(onboarding);
            return;
        }

        validateInput(consentPutUnifiedDTO);

        InitiativeDTO initiativeDTO = getInitiative(consentPutUnifiedDTO.getInitiativeId());
        onboarding = new Onboarding(consentPutUnifiedDTO.getInitiativeId(), userId);

        checkDates(initiativeDTO, onboarding);
        checkBudget(initiativeDTO, onboarding);

        if (hasAutomatedCriteriaAndPdndNotAccepted(initiativeDTO, consentPutUnifiedDTO)) {
            handlePdndDenied(onboarding, userId, initiativeDTO, startTime);
        }

        selfDeclaration(initiativeDTO, consentPutUnifiedDTO, userId);

        fillOnboardingData(onboarding, consentPutUnifiedDTO);
        onboarding.setUserMail(consentPutUnifiedDTO.isWebChannel() ? consentPutUnifiedDTO.getUserMail() : null);


        OnboardingDTO onboardingDTO = consentMapper.map(onboarding);
        onboardingDTO.setServiceId(initiativeDTO.getAdditionalInfo().getServiceId());

        onboardingProducer.sendSaveConsent(onboardingDTO);
        onboardingRepository.save(onboarding);

        performanceLog(startTime, "SAVE_CONSENT", userId, initiativeDTO.getInitiativeId());
    }

    @Override
    public void handleExistingOnboarding(Onboarding onboarding) {
        if (OnboardingWorkflowConstants.STATUS_IDEMPOTENT.contains(onboarding.getStatus())) {
            return;
        }
        checkStatus(onboarding);
    }

    @Override
    public void validateInput(ConsentPutUnifiedDTO dto) {
        if (ChannelType.WEB.equals(dto.getChannel()) &&
                (dto.getUserMail() == null ||
                        dto.getUserMailConfirmation() == null ||
                        !dto.getUserMail().trim().equalsIgnoreCase(dto.getUserMailConfirmation().trim()))) {
            throw new EmailNotMatchedException(EMAIL_NOT_MATCHED_MSG);
        }

        if (dto.getConfirmedTos() == null || !dto.getConfirmedTos()) {
            throw new TosNotConfirmedException(TOS_NOT_CONFIRMED_MSG);
        }
    }


    @Override
    public boolean hasAutomatedCriteriaAndPdndNotAccepted(InitiativeDTO initiativeDTO, ConsentPutUnifiedDTO dto) {
        return !initiativeDTO.getBeneficiaryRule().getAutomatedCriteria().isEmpty() && !dto.isPdndAccept();
    }


    @Override
    public void handlePdndDenied(Onboarding onboarding, String userId, InitiativeDTO initiativeDTO, long startTime) {
        performanceLog(startTime, "SAVE_CONSENT", userId, initiativeDTO.getInitiativeId());
        auditUtilities.logOnboardingKOWithReason(userId, initiativeDTO.getInitiativeId(), onboarding.getChannel(),
                OnboardingWorkflowConstants.ERROR_PDND_AUDIT);
        onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
        onboarding.setDetailKO(PDND_CONSENT_DENIED);
        onboardingRepository.save(onboarding);
        throw new PDNDConsentDeniedException(String.format(ERROR_PDND_MSG, initiativeDTO.getInitiativeId()));
    }

    @Override
    public void fillOnboardingData(Onboarding onboarding, ConsentPutUnifiedDTO dto) {
        onboarding.setStatus(OnboardingWorkflowConstants.ON_EVALUATION);
        onboarding.setPdndAccept(dto.isPdndAccept());
        onboarding.setTc(dto.getConfirmedTos());

        LocalDateTime now = LocalDateTime.now();
        onboarding.setCriteriaConsensusTimestamp(now);
        onboarding.setUpdateDate(now);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        OnboardingServiceWebImpl that = (OnboardingServiceWebImpl) o;
        return initiativeWebMapper.equals(that.initiativeWebMapper);
    }

    @Override
    public int hashCode() {
        return initiativeWebMapper.hashCode();
    }

}
