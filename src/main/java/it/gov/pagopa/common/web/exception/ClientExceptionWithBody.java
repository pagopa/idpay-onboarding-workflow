package it.gov.pagopa.common.web.exception;

import lombok.Getter;
import org.springframework.http.HttpStatus;

@Getter
public class ClientExceptionWithBody extends ClientException{
  private final Integer code;
  private final String details;

  public ClientExceptionWithBody(HttpStatus httpStatus, Integer code, String message, String details){
    this(httpStatus, code, message, details, null);
  }

  public ClientExceptionWithBody(HttpStatus httpStatus, Integer code, String message,
      String details, Throwable ex){
    this(httpStatus, code, message, details, false, ex);
  }

  public ClientExceptionWithBody(HttpStatus httpStatus, Integer code, String message, String details, boolean printStackTrace, Throwable ex){
    super(httpStatus, message, printStackTrace, ex);
    this.code = code;
    this.details = details;
  }
}
