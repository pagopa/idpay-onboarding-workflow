package it.gov.pagopa.common.web.exception;

import static org.mockito.Mockito.doThrow;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mongodb.MongoQueryException;
import com.mongodb.ServerAddress;
import lombok.extern.slf4j.Slf4j;
import org.bson.BsonDocument;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.data.mongodb.UncategorizedMongoDbException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@ExtendWith(SpringExtension.class)
@WebMvcTest(value = {MongoExceptionHandlerTest.TestController.class}, excludeAutoConfiguration = SecurityAutoConfiguration.class)
@ContextConfiguration(classes = {MongoExceptionHandler.class, MongoExceptionHandlerTest.TestController.class})
class MongoExceptionHandlerTest {

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
    String testEndpoint(){
      return "OK";
    }
  }


  @Test
  void testHandleUncategorizedMongoDbException() throws Exception {

    String mongoFullErrorResponse = """
        {"ok": 0.0, "errmsg": "Error=16500, RetryAfterMs=34,\s
        Details='Response status code does not indicate success: TooManyRequests (429) Substatus: 3200 ActivityId: 46ba3855-bc3b-4670-8609-17e1c2c87778 Reason:\s
        (\\r\\nErrors : [\\r\\n \\"Request rate is large. More Request Units may be needed, so no changes were made. Please retry this request later. Learn more:
         http://aka.ms/cosmosdb-error-429\\"\\r\\n]\\r\\n) ", "code": 16500, "codeName": "RequestRateTooLarge"}
        """;

    final MongoQueryException mongoQueryException = new MongoQueryException(
        BsonDocument.parse(mongoFullErrorResponse), new ServerAddress());
    doThrow(new UncategorizedMongoDbException(mongoQueryException.getMessage(), mongoQueryException))
        .when(testControllerSpy).testEndpoint();

    mockMvc.perform(MockMvcRequestBuilders.get("/test")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isTooManyRequests())
        .andExpect(MockMvcResultMatchers.jsonPath("$.code").value("TOO_MANY_REQUESTS"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.message").value("TOO_MANY_REQUESTS"))
        .andExpect(MockMvcResultMatchers.header().exists(HttpHeaders.RETRY_AFTER))
        .andExpect(MockMvcResultMatchers.header().string(HttpHeaders.RETRY_AFTER, "1"))
        .andExpect(MockMvcResultMatchers.header().string("Retry-After-Ms", "34"));
  }

}
