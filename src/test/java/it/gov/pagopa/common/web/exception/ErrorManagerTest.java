package it.gov.pagopa.common.web.exception;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@ExtendWith(SpringExtension.class)
@WebMvcTest(value = {ErrorManagerTest.TestController.class}, excludeAutoConfiguration = SecurityAutoConfiguration.class)
@ContextConfiguration(classes = {ErrorManagerTest.TestController.class, ErrorManager.class})
class ErrorManagerTest {

  @Autowired
  private MockMvc mockMvc;

  @Autowired
  private ObjectMapper objectMapper;

  @SpyBean
  private TestController testControllerSpy;

  @RestController
  @Slf4j
  static class TestController {

    @GetMapping("/test")
    String testEndpoint() {
      return "OK";
    }
  }

  @Test
  void handleExceptionClientExceptionNoBody() throws Exception {
    Mockito.doThrow(new ClientExceptionNoBody(HttpStatus.BAD_REQUEST, "NOTFOUND ClientExceptionNoBody"))
        .when(testControllerSpy).testEndpoint();

    mockMvc.perform(MockMvcRequestBuilders.get("/test")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isBadRequest());
  }

  @Test
  void handleExceptionClientExceptionWithBody() throws Exception {
    Mockito.doThrow(new ClientExceptionWithBody(HttpStatus.BAD_REQUEST, "Error","Error ClientExceptionWithBody", ""))
        .when(testControllerSpy).testEndpoint();

    mockMvc.perform(MockMvcRequestBuilders.get("/test")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isBadRequest())
        .andExpect(MockMvcResultMatchers.content().json("{\"code\":\"Error\",\"message\":\"Error ClientExceptionWithBody\",\"details\":\"\"}"));

    Mockito.doThrow(new ClientExceptionWithBody(HttpStatus.BAD_REQUEST, "Error","Error ClientExceptionWithBody","", new Exception()))
        .when(testControllerSpy).testEndpoint();

    mockMvc.perform(MockMvcRequestBuilders.get("/test")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isBadRequest())
        .andExpect(MockMvcResultMatchers.content().json("{\"code\":\"Error\",\"message\":\"Error ClientExceptionWithBody\",\"details\":\"\"}"));
  }

  @Test
  void handleExceptionClientExceptionTest() throws Exception {

    Mockito.doThrow(ClientException.class)
        .when(testControllerSpy).testEndpoint();

    mockMvc.perform(MockMvcRequestBuilders.get("/test")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isInternalServerError())
        .andExpect(MockMvcResultMatchers.content().json("{\"code\":\"Error\",\"message\":\"Something gone wrong\",\"details\":\"\"}"));

    Mockito.doThrow(new ClientException(HttpStatus.BAD_REQUEST, "ClientException with httpStatus and message"))
        .when(testControllerSpy).testEndpoint();

    mockMvc.perform(MockMvcRequestBuilders.get("/test")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isInternalServerError())
        .andExpect(MockMvcResultMatchers.content().json("{\"code\":\"Error\",\"message\":\"Something gone wrong\",\"details\":\"\"}"));

    Mockito.doThrow(new ClientException(HttpStatus.BAD_REQUEST, "ClientException with httpStatus, message and throwable", new Throwable()))
        .when(testControllerSpy).testEndpoint();

    mockMvc.perform(MockMvcRequestBuilders.get("/test")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isInternalServerError())
        .andExpect(MockMvcResultMatchers.content().json("{\"code\":\"Error\",\"message\":\"Something gone wrong\",\"details\":\"\"}"));
  }

  @Test
  void handleExceptionRuntimeException() throws Exception {
    Mockito.doThrow(RuntimeException.class)
        .when(testControllerSpy).testEndpoint();

    mockMvc.perform(MockMvcRequestBuilders.get("/test")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isInternalServerError())
        .andExpect(MockMvcResultMatchers.content().json("{\"code\":\"Error\",\"message\":\"Something gone wrong\",\"details\":\"\"}"));
  }
}
