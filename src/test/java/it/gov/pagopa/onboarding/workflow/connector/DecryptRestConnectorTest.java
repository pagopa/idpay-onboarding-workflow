package it.gov.pagopa.onboarding.workflow.connector;

import it.gov.pagopa.onboarding.workflow.connector.decrypt.DecryptRest;
import it.gov.pagopa.onboarding.workflow.connector.decrypt.DecryptRestConnectorImpl;
import it.gov.pagopa.onboarding.workflow.dto.DecryptCfDTO;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {DecryptRestConnectorImpl.class, String.class})
@ExtendWith(SpringExtension.class)
class DecryptRestConnectorTest {
    @MockBean
    private DecryptRest decryptRest;

    @Autowired
    private DecryptRestConnectorImpl decryptRestConnectorImpl;

    @Test
    void testGetPiiByToken() {
        DecryptCfDTO decryptCfDTO = new DecryptCfDTO("Pii");
        when(decryptRest.getPiiByToken(any(), any())).thenReturn(decryptCfDTO);
        assertSame(decryptCfDTO, decryptRestConnectorImpl.getPiiByToken("ABC123"));
        verify(decryptRest).getPiiByToken(any(), any());
    }
}

