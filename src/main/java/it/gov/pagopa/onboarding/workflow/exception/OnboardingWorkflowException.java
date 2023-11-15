package it.gov.pagopa.onboarding.workflow.exception;

import it.gov.pagopa.common.web.exception.ClientExceptionWithBody;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;

@Getter
@Slf4j
@SuppressWarnings("squid:S110")
public class OnboardingWorkflowException extends ClientExceptionWithBody {

    public OnboardingWorkflowException(int code, String message, String details) {
        this(code, message, details, null);
    }

    public OnboardingWorkflowException(int code, String message, String details, Throwable e) {
        //super(transcodeHttpStatus(code, e), code, message, details, e);
        super(transcodeHttpStatus(code, e), "CODICE", message, e);
    }

    @NonNull
    private static HttpStatus transcodeHttpStatus(int code, Throwable ex) {
        try {
            return HttpStatus.valueOf(code);
        } catch (IllegalArgumentException e) {
            log.info("Obtained unexpected error code: {}, exception message: {}", code, ex != null ? ex.getMessage() : "null");
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }
    }
}
