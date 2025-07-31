package it.gov.pagopa.onboarding.workflow.service.common;

import it.gov.pagopa.onboarding.workflow.dto.ConsentPutDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.InitiativeDTO;
import it.gov.pagopa.onboarding.workflow.dto.initiative.SelfCriteriaMultiDTO;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.model.SelfDeclaration;

import java.time.LocalDate;
import java.util.Map;

public interface OnboardingServiceCommon {

    Onboarding findByInitiativeIdAndUserId(String initiativeId, String userId);
    InitiativeDTO getInitiative(String initiativeId);
    void checkStatus(Onboarding onboarding);
    void checkDates(InitiativeDTO initiativeDTO, Onboarding onboarding);
    LocalDate getEndDate(InitiativeDTO initiativeDTO, Onboarding onboarding);
    void checkBudget(InitiativeDTO initiativeDTO, Onboarding onboarding);
    void performanceLog(long startTime, String service, String userId, String initiativeId);
    void selfDeclaration(InitiativeDTO initiativeDTO, ConsentPutDTO consentPutDTO, String userId);
    boolean sizeCheck(InitiativeDTO initiativeDTO, Map<String, Boolean> selfDeclarationBool, Map<String, String> selfDeclarationMulti, Map<String, String> selfDeclarationText);
    void multiCriteriaCheck(InitiativeDTO initiativeDTO, SelfCriteriaMultiDTO multi, Map<String, String> selfDeclarationMulti);
    SelfDeclaration getOrCreateSelfDeclaration(String initiativeId, String userId);

}
