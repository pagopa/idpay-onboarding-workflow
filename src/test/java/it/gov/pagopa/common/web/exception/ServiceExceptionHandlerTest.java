package it.gov.pagopa.common.web.exception;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@ExtendWith(SpringExtension.class)
@WebMvcTest(value = {
        ServiceExceptionHandlerTest.TestController.class}, excludeAutoConfiguration = SecurityAutoConfiguration.class)
@ContextConfiguration(classes = {ServiceExceptionHandler.class,
        ServiceExceptionHandlerTest.TestController.class, ErrorManager.class})
class ServiceExceptionHandlerTest {
    @Autowired
    private MockMvc mockMvc;

    @RestController
    @Slf4j
    static class TestController {

        @GetMapping("/test")
        String testEndpoint() {
            throw new ServiceException("DUMMY_CODE", "DUMMY_MESSAGE", new ErrorPayloadTest("RESPONSE",0));
        }
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    static class ErrorPayloadTest implements ServiceExceptionPayload {
        private String stringCode;
        private long longCode;
    }

    @Test
    void handleErrorResponseDTO() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.get("/test")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(MockMvcResultMatchers.status().isInternalServerError())
                .andExpect(MockMvcResultMatchers.content().json("{\"stringCode\":\"RESPONSE\",\"longCode\":0}", false));
    }
}
