package it.gov.pagopa.onboarding.workflow.config;

import it.gov.pagopa.common.web.exception.ServiceException;
import it.gov.pagopa.onboarding.workflow.exception.custom.badrequest.OperationNotAllowedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.badrequest.PageSizeNotAllowedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.forbidden.*;
import it.gov.pagopa.onboarding.workflow.exception.custom.notfound.InitiativeNotFoundException;
import it.gov.pagopa.onboarding.workflow.exception.custom.notfound.UserNotOnboardedException;
import it.gov.pagopa.onboarding.workflow.exception.custom.servererror.AdmissibilityInvocationException;
import it.gov.pagopa.onboarding.workflow.exception.custom.servererror.InitiativeInvocationException;
import it.gov.pagopa.onboarding.workflow.exception.custom.servererror.PDVInvocationException;
import it.gov.pagopa.onboarding.workflow.exception.custom.servererror.UserSuspensionOrReadmissionException;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;

import java.util.HashMap;
import java.util.Map;

@Configuration
public class ServiceExceptionConfig {

    @Bean
    public Map<Class<? extends ServiceException>, HttpStatus> serviceExceptionMapper() {
        Map<Class<? extends ServiceException>, HttpStatus> exceptionMap = new HashMap<>();

        // BadRequest
        exceptionMap.put(PageSizeNotAllowedException.class, HttpStatus.BAD_REQUEST);
        exceptionMap.put(OperationNotAllowedException.class, HttpStatus.BAD_REQUEST);

        // Forbidden
        exceptionMap.put(UserNotInWhitelistException.class, HttpStatus.FORBIDDEN);
        exceptionMap.put(InitiativeInvalidException.class, HttpStatus.FORBIDDEN);
        exceptionMap.put(InitiativeBudgetExhaustedException.class, HttpStatus.FORBIDDEN);
        exceptionMap.put(PDNDConsentDeniedException.class, HttpStatus.FORBIDDEN);
        exceptionMap.put(SelfDeclarationCrtieriaException.class, HttpStatus.FORBIDDEN);
        exceptionMap.put(UserUnsubscribedException.class, HttpStatus.FORBIDDEN);
        exceptionMap.put(InitiativeOnboardingException.class, HttpStatus.FORBIDDEN);

        // NotFound
        exceptionMap.put(UserNotOnboardedException.class, HttpStatus.NOT_FOUND);
        exceptionMap.put(InitiativeNotFoundException.class, HttpStatus.NOT_FOUND);

        // InternalServerError
        exceptionMap.put(UserSuspensionOrReadmissionException.class, HttpStatus.INTERNAL_SERVER_ERROR);
        exceptionMap.put(PDVInvocationException.class, HttpStatus.INTERNAL_SERVER_ERROR);
        exceptionMap.put(InitiativeInvocationException.class, HttpStatus.INTERNAL_SERVER_ERROR);
        exceptionMap.put(AdmissibilityInvocationException.class, HttpStatus.INTERNAL_SERVER_ERROR);

        return exceptionMap;
    }
}
