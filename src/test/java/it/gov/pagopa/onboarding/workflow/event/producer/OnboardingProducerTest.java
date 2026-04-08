package it.gov.pagopa.onboarding.workflow.event.producer;

import com.azure.spring.messaging.servicebus.support.ServiceBusMessageHeaders;
import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.messaging.Message;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.Mockito.eq;

import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class OnboardingProducerTest {

    @Mock
    private StreamBridge streamBridge;

    @InjectMocks
    private OnboardingProducer onboardingProducer;

    @InjectMocks
    private ConsentMapper consentMapper;


    private final static String USER_ID = "USERID";
    private final static String INITIATIVE_ID = "INITIATIVEID";

    @BeforeEach
    void init() {
        ReflectionTestUtils.setField(onboardingProducer, "binder", "testBinder");
    }


    @Test
    void testSendSaveConsent() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        OnboardingDTO onboardingDTO = consentMapper.map(onboarding);

        onboardingProducer.sendSaveConsent(onboardingDTO);

        verify(streamBridge).send(
                eq("onboarding-out-1"),
                eq("testBinder"),
                argThat((Message<?> msg) ->
                        msg.getPayload().equals(onboardingDTO) &&
                                INITIATIVE_ID.equals(msg.getHeaders().get(ServiceBusMessageHeaders.SESSION_ID))
                )
        );

    }

}



