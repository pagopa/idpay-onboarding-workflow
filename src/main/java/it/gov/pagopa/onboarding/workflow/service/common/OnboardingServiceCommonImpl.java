package it.gov.pagopa.onboarding.workflow.service.common;

import it.gov.pagopa.onboarding.workflow.connector.InitiativeRestConnector;
import it.gov.pagopa.onboarding.workflow.connector.admissibility.AdmissibilityRestConnector;
import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.dto.ConsentPutUnifiedDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.SelfConsentTextDTO;
import it.gov.pagopa.onboarding.workflow.dto.admissibility.InitiativeStatusDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaBoolDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaMultiDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaTextDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.event.producer.OnboardingProducer;
import it.gov.pagopa.onboarding.workflow.exception.custom.*;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.model.SelfDeclaration;
import it.gov.pagopa.onboarding.workflow.model.SelfDeclarationMultiValues;
import it.gov.pagopa.onboarding.workflow.model.SelfDeclarationTextValues;
import it.gov.pagopa.onboarding.workflow.repository.OnboardingRepository;
import it.gov.pagopa.onboarding.workflow.repository.SelfDeclarationRepository;
import it.gov.pagopa.onboarding.workflow.utils.AuditUtilities;
import it.gov.pagopa.onboarding.workflow.utils.Utilities;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionCode.*;
import static it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants.ExceptionMessage.*;

@Slf4j
@Data
public class OnboardingServiceCommonImpl implements OnboardingServiceCommon{

    protected final AuditUtilities auditUtilities;
    protected final Utilities utilities;
    protected final OnboardingRepository onboardingRepository;
    protected final AdmissibilityRestConnector admissibilityRestConnector;
    protected final SelfDeclarationRepository selfDeclarationRepository;
    protected final ConsentMapper consentMapper;
    protected final OnboardingProducer onboardingProducer;
    protected final InitiativeRestConnector initiativeRestConnector;

    public OnboardingServiceCommonImpl(AuditUtilities auditUtilities,
                                       Utilities utilities,
                                       OnboardingRepository onboardingRepository,
                                       AdmissibilityRestConnector admissibilityRestConnector,
                                       SelfDeclarationRepository selfDeclarationRepository,
                                       ConsentMapper consentMapper,
                                       OnboardingProducer onboardingProducer,
                                       InitiativeRestConnector initiativeRestConnector) {
        this.auditUtilities = auditUtilities;
        this.utilities = utilities;
        this.onboardingRepository = onboardingRepository;
        this.admissibilityRestConnector = admissibilityRestConnector;
        this.selfDeclarationRepository = selfDeclarationRepository;
        this.consentMapper = consentMapper;
        this.onboardingProducer = onboardingProducer;
        this.initiativeRestConnector = initiativeRestConnector;
    }


    @Override
    public Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId) {
        return onboardingRepository.findById(Onboarding.buildId(initiativeId, userId))
                .orElseThrow(() -> new UserNotOnboardedException(String.format(ID_S_NOT_FOUND_MSG, initiativeId)));
    }

    @Override
    public Onboarding findOnboardingByInitiativeIdAndUserId(String initiativeId, String userId) {
        return onboardingRepository.findById(Onboarding.buildId(initiativeId, userId)).orElse(null);
    }

    @Override
    public InitiativeDTO getInitiative(String initiativeId) {
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


    @Override
    public void checkStatus(Onboarding onboarding) {
        String status = onboarding.getStatus();
        if (List.of(OnboardingWorkflowConstants.ONBOARDING_KO, OnboardingWorkflowConstants.ELIGIBLE_KO).contains(status) &&
                !OnboardingWorkflowConstants.REJECTION_REASON_BIRTHDATE_KO.equals(onboarding.getDetailKO())){
            auditUtilities.logOnboardingKOWithReason(onboarding.getUserId(), onboarding.getInitiativeId(), onboarding.getChannel(),
                    utilities.getMessageOnboardingKO(onboarding.getDetailKO()));
            utilities.throwOnboardingKOException(onboarding.getDetailKO(), onboarding.getInitiativeId());
        }
        if (status.equals(OnboardingWorkflowConstants.STATUS_UNSUBSCRIBED)) {
            auditUtilities.logOnboardingKOWithReason(onboarding.getUserId(), onboarding.getInitiativeId(), onboarding.getChannel(),
                    OnboardingWorkflowConstants.ERROR_UNSUBSCRIBED_INITIATIVE_AUDIT);
            throw new UserUnsubscribedException(String.format(ERROR_UNSUBSCRIBED_INITIATIVE_MSG, onboarding.getInitiativeId()));
        }
    }

    @Override
    public void checkDates(InitiativeDTO initiativeDTO, Onboarding onboarding) {
        LocalDate requestDate = LocalDate.now();

        LocalDate startDate =
                (initiativeDTO.getGeneral().getRankingStartDate() != null) ? initiativeDTO.getGeneral()
                        .getRankingStartDate() : initiativeDTO.getGeneral()
                        .getStartDate();

        LocalDate endDate = getEndDate(initiativeDTO, onboarding);

        if (requestDate.isBefore(startDate)){
            auditUtilities.logOnboardingKOWithReason(onboarding.getInitiativeId(), onboarding.getUserId(), onboarding.getChannel(),
                    OnboardingWorkflowConstants.ERROR_INITIATIVE_NOT_STARTED_MSG_AUDIT);
            throw new InitiativeInvalidException(INITIATIVE_NOT_STARTED,
                    String.format(ERROR_INITIATIVE_NOT_STARTED_MSG, initiativeDTO.getInitiativeId()));
        }

        if (requestDate.isAfter(endDate)){
            LocalDateTime localDateTime = LocalDateTime.now();
            onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
            onboarding.setOnboardingKODate(localDateTime);
            onboarding.setUpdateDate(localDateTime);
            onboarding.setDetailKO(OnboardingWorkflowConstants.ERROR_INITIATIVE_END);
            onboardingRepository.save(onboarding);
            auditUtilities.logOnboardingKOWithReason(onboarding.getUserId(), onboarding.getInitiativeId(), onboarding.getChannel(),
                    OnboardingWorkflowConstants.ERROR_INITIATIVE_END_MSG_AUDIT);
            throw new InitiativeInvalidException(INITIATIVE_ENDED,
                    String.format(ERROR_INITIATIVE_END_MSG, initiativeDTO.getInitiativeId()));
        }
    }

    @Override
    public LocalDate getEndDate(InitiativeDTO initiativeDTO, Onboarding onboarding) {
        LocalDate endDate = initiativeDTO.getGeneral()
                .getEndDate();

        if(initiativeDTO.getGeneral().getRankingEndDate() != null &&
                (!OnboardingWorkflowConstants.BENEFICIARY_TYPE_NF.equals(initiativeDTO.getGeneral().getBeneficiaryType())
                        || (!initiativeDTO.getGeneral().getRankingEnabled()
                        && !(OnboardingWorkflowConstants.DEMANDED.equals(onboarding.getStatus())
                        || onboarding.getDemandedDate() != null)))
        ){
            endDate = initiativeDTO.getGeneral().getRankingEndDate();
        }
        return endDate;
    }

    @Override
    public void checkBudget(InitiativeDTO initiativeDTO, Onboarding onboarding) {
        if(Boolean.TRUE.equals(initiativeDTO.getGeneral().getBeneficiaryKnown())){
            return;
        }
        InitiativeStatusDTO initiativeStatusDTO = admissibilityRestConnector.getInitiativeStatus(
                initiativeDTO.getInitiativeId());
        if (initiativeStatusDTO.isBudgetAvailable() && initiativeStatusDTO.getStatus()
                .equals(OnboardingWorkflowConstants.PUBLISHED)) {
            return;
        }
        LocalDateTime localDateTime = LocalDateTime.now();
        onboarding.setStatus(OnboardingWorkflowConstants.ONBOARDING_KO);
        onboarding.setOnboardingKODate(localDateTime);
        onboarding.setUpdateDate(localDateTime);
        onboarding.setDetailKO(OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED);
        onboardingRepository.save(onboarding);
        auditUtilities.logOnboardingKOWithReason(onboarding.getInitiativeId(),
                onboarding.getUserId(), onboarding.getChannel(),
                OnboardingWorkflowConstants.ERROR_BUDGET_TERMINATED_MSG_AUDIT);
        throw new InitiativeBudgetExhaustedException(String.format(ERROR_BUDGET_TERMINATED_MSG, initiativeDTO.getInitiativeId()));
    }

    @Override
    public void performanceLog(long startTime, String service, String userId, String initiativeId) {
        log.info(
                "[PERFORMANCE_LOG] [{}] Time occurred to perform business logic: {} ms on initiativeId: {}, and userId: {}",
                service,
                System.currentTimeMillis() - startTime,
                initiativeId,
                userId);
    }

    @Override
    public void selfDeclaration(InitiativeDTO initiativeDTO, ConsentPutUnifiedDTO consentPutDTO, String userId) {
        if (initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria().isEmpty()) {
            return;
        }

        Map<String, Boolean> selfDeclarationBool = consentPutDTO.getSelfDeclarationList().stream()
                .filter(item -> item.getClass().equals(SelfConsentBoolDTO.class))
                .map(SelfConsentBoolDTO.class::cast)
                .collect(Collectors.toMap(SelfConsentBoolDTO::getCode, SelfConsentBoolDTO::isAccepted));

        Map<String, String> selfDeclarationMulti = consentPutDTO.getSelfDeclarationList().stream()
                .filter(item -> item.getClass().equals(SelfConsentMultiDTO.class))
                .map(SelfConsentMultiDTO.class::cast)
                .collect(Collectors.toMap(SelfConsentMultiDTO::getCode, SelfConsentMultiDTO::getValue));

        Map<String, String> selfDeclarationText = consentPutDTO.getSelfDeclarationList().stream()
                .filter(item -> item.getClass().equals(SelfConsentTextDTO.class))
                .map(SelfConsentTextDTO.class::cast)
                .collect(Collectors.toMap(SelfConsentTextDTO::getCode, SelfConsentTextDTO::getValue));

        if (sizeCheck(initiativeDTO, selfDeclarationBool, selfDeclarationMulti, selfDeclarationText)) {
            auditUtilities.logOnboardingKOInitiativeId(initiativeDTO.getInitiativeId(), OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_SIZE_AUDIT);
            throw new SelfDeclarationCrtieriaException(String.format(ERROR_SELF_DECLARATION_NOT_VALID_MSG, initiativeDTO.getInitiativeId()));
        }

        initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria().forEach(item -> {
            if (item instanceof SelfCriteriaBoolDTO bool) {
                Boolean flag = selfDeclarationBool.get(bool.getCode());
                if (flag == null || !flag) {
                    auditUtilities.logOnboardingKOInitiativeId(initiativeDTO.getInitiativeId(), OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY_AUDIT);
                    throw new SelfDeclarationCrtieriaException(String.format(ERROR_SELF_DECLARATION_NOT_VALID_MSG, initiativeDTO.getInitiativeId()));
                }
                bool.setValue(true);
            }
            if (item instanceof SelfCriteriaMultiDTO multi) {
                multiCriteriaCheck(initiativeDTO, multi, selfDeclarationMulti);

                SelfDeclarationMultiValues multiValueToSave = new SelfDeclarationMultiValues(
                        multi.getType(),
                        multi.getDescription(),
                        multi.getValue(),
                        multi.getCode()
                );

                SelfDeclaration selfDeclarationToSave = getOrCreateSelfDeclaration(initiativeDTO.getInitiativeId(), userId);

                selfDeclarationToSave.getSelfDeclarationMultiValues().add(multiValueToSave);

                selfDeclarationRepository.save(selfDeclarationToSave);
            }
            if (item instanceof SelfCriteriaTextDTO text) {
                String value = selfDeclarationText.get(text.getCode());
                if (value == null) {
                    auditUtilities.logOnboardingKOInitiativeId(initiativeDTO.getInitiativeId(), OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY_AUDIT);
                    throw new SelfDeclarationCrtieriaException(String.format(ERROR_SELF_DECLARATION_NOT_VALID_MSG, initiativeDTO.getInitiativeId()));
                }
                text.setValue(value);

                SelfDeclaration selfDeclarationToSave = getOrCreateSelfDeclaration(initiativeDTO.getInitiativeId(), userId);

                SelfDeclarationTextValues selfDeclarationValues = new SelfDeclarationTextValues(
                        text.getType(),
                        text.getDescription(),
                        text.getValue(),
                        text.getCode()
                );

                selfDeclarationToSave.getSelfDeclarationTextValues().add(selfDeclarationValues);

                selfDeclarationRepository.save(selfDeclarationToSave);
            }
        });
    }

    @Override
    public boolean sizeCheck(InitiativeDTO initiativeDTO, Map<String, Boolean> selfDeclarationBool, Map<String, String> selfDeclarationMulti, Map<String, String> selfDeclarationText) {
        return selfDeclarationBool.size() + selfDeclarationMulti.size() + selfDeclarationText.size()
                != initiativeDTO.getBeneficiaryRule().getSelfDeclarationCriteria().size();
    }

    @Override
    public void multiCriteriaCheck(InitiativeDTO initiativeDTO, SelfCriteriaMultiDTO multi, Map<String, String> selfDeclarationMulti) {
        String value = selfDeclarationMulti.get(multi.getCode());
        if (value == null || !multi.getValue().contains(value)) {
            auditUtilities.logOnboardingKOInitiativeId(initiativeDTO.getInitiativeId(), OnboardingWorkflowConstants.ERROR_SELF_DECLARATION_DENY_AUDIT);
            throw new SelfDeclarationCrtieriaException(String.format(ERROR_SELF_DECLARATION_NOT_VALID_MSG, initiativeDTO.getInitiativeId()));
        }
        multi.setValue(List.of(value));
    }

    @Override
    public SelfDeclaration getOrCreateSelfDeclaration(String initiativeId, String userId) {
        return selfDeclarationRepository.findById(SelfDeclaration.buildId(initiativeId, userId))
                .orElse(SelfDeclaration.builder()
                        .id(SelfDeclaration.buildId(initiativeId, userId))
                        .initiativeId(initiativeId)
                        .userId(userId)
                        .selfDeclarationTextValues(new ArrayList<>())
                        .selfDeclarationMultiValues(new ArrayList<>())
                        .build());
    }


}
