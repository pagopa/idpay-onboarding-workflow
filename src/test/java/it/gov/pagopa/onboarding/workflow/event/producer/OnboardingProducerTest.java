package it.gov.pagopa.onboarding.workflow.event.producer;

import it.gov.pagopa.onboarding.workflow.dto.OnboardingDTO;
import it.gov.pagopa.onboarding.workflow.dto.mapper.ConsentMapper;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.cloud.stream.function.StreamBridge;

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
    @Test
    void testSendSaveConsent() {
        Onboarding onboarding = new Onboarding(INITIATIVE_ID, USER_ID);
        OnboardingDTO onboardingDTO = consentMapper.map(onboarding);
        onboardingProducer.sendSaveConsent(onboardingDTO);
        verify(streamBridge).send(Mockito.eq("onboarding-out-1"), Mockito.any(), Mockito.eq(onboardingDTO));
    }
}
