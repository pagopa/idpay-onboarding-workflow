package it.gov.pagopa.common.wiremock;


import it.gov.pagopa.common.rest.utils.WireMockUtils;
import it.gov.pagopa.common.utils.JUnitExtensionContextHolder;
import it.gov.pagopa.common.utils.TestUtils;
import jakarta.annotation.PostConstruct;
import lombok.NonNull;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContextInitializer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.env.EnumerablePropertySource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.support.TestPropertySourceUtils;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

@ExtendWith(JUnitExtensionContextHolder.class)
@SpringBootTest
@ContextConfiguration(
        initializers = {BaseWireMockTest.WireMockInitializer.class})
@SuppressWarnings("squid:S2187")
public class BaseWireMockTest {
    public static final String WIREMOCK_TEST_PROP2BASEPATH_MAP_PREFIX =  "wireMock-test.prop2basePath.";
    public static final String WIREMOCK_TEST_PROP2BASEPATH_SECURE_MAP_PREFIX =  "wireMock-test.prop2basePath.secure.";

    private static final Map<String,String> propertiesMap = new HashMap<>();
    private static final Map<String,String> propertiesSecureMap = new HashMap<>();

    @PostConstruct
    public void logEmbeddedServerConfig() {
        String wiremockHttpBaseUrl = "UNKNOWN";
        String wiremockHttpsBaseUrl = "UNKNOWN";
        try {
            wiremockHttpBaseUrl = serverWireMockExtension.getRuntimeInfo().getHttpBaseUrl();
            wiremockHttpsBaseUrl = serverWireMockExtension.getRuntimeInfo().getHttpsBaseUrl();
        } catch (Exception e) {
            System.out.println("Cannot read wiremock urls");
        }
        System.out.printf("""
                        ************************
                        WireMock http: %s
                        WireMock https: %s
                        ************************
                        """,
                wiremockHttpBaseUrl,
                wiremockHttpsBaseUrl);
    }

    //region desc=Setting WireMock
    private static boolean WIREMOCK_REQUEST_CLIENT_AUTH = true;
    private static boolean USE_TRUSTORE_OK = true;
    public static final String TRUSTSTORE_PATH = "src/test/resources/wiremockKeyStore.p12";
    private static final String TRUSTSTORE_KO_PATH = "src/test/resources/wiremockTrustStoreKO.p12";
    @RegisterExtension
    static com.github.tomakehurst.wiremock.junit5.WireMockExtension serverWireMockExtension = initServerWiremock();

    public static void configureServerWiremockBeforeAll(boolean needClientAuth, boolean useTrustoreOk) {
        WIREMOCK_REQUEST_CLIENT_AUTH = needClientAuth;
        USE_TRUSTORE_OK = useTrustoreOk;
        initServerWiremock();
    }

    private static com.github.tomakehurst.wiremock.junit5.WireMockExtension initServerWiremock() {
        int httpPort=0;
        int httpsPort=0;
        boolean start=false;

        // re-using shutdown server port in order to let Spring loaded configuration still valid
        if (serverWireMockExtension != null && JUnitExtensionContextHolder.extensionContext != null) {
            try {
                httpPort = serverWireMockExtension.getRuntimeInfo().getHttpPort();
                httpsPort = serverWireMockExtension.getRuntimeInfo().getHttpsPort();

                serverWireMockExtension.shutdownServer();
                // waiting server stop, releasing ports
                TestUtils.wait(200, TimeUnit.MILLISECONDS);
                start=true;
            } catch (IllegalStateException e){
                // Do Nothing: the wiremock server was not started
            }
        }

        com.github.tomakehurst.wiremock.junit5.WireMockExtension newWireMockConfig = WireMockUtils.initServerWiremock(
                httpPort,
                httpsPort,
                "src/test/resources/stub",
                WIREMOCK_REQUEST_CLIENT_AUTH,
                USE_TRUSTORE_OK ? TRUSTSTORE_PATH : TRUSTSTORE_KO_PATH,
                "idpay");

        if(start){
            try {
                newWireMockConfig.beforeAll(JUnitExtensionContextHolder.extensionContext);
            } catch (Exception e) {
                throw new IllegalStateException("Cannot start WireMock JUnit Extension", e);
            }
        }

        return serverWireMockExtension = newWireMockConfig;
    }

    @AfterAll
    static void restoreWireMockConfig() {
        if(!USE_TRUSTORE_OK || !WIREMOCK_REQUEST_CLIENT_AUTH) {
            USE_TRUSTORE_OK = true;
            WIREMOCK_REQUEST_CLIENT_AUTH = true;
            initServerWiremock();
        }
    }

    public static class WireMockInitializer implements ApplicationContextInitializer<ConfigurableApplicationContext> {
        @Override
        public void initialize(@NonNull ConfigurableApplicationContext applicationContext) {

            applicationContext.getEnvironment().getPropertySources().stream()
                    .filter(propertySource -> propertySource instanceof EnumerablePropertySource)
                    .map(propertySource -> (EnumerablePropertySource<?>) propertySource)
                    .flatMap(propertySource -> Arrays.stream(propertySource.getPropertyNames()))
                    .forEach(key -> {
                        if (key.startsWith(WIREMOCK_TEST_PROP2BASEPATH_MAP_PREFIX)) {
                            propertiesMap.put(key.substring(WIREMOCK_TEST_PROP2BASEPATH_MAP_PREFIX.length()), applicationContext.getEnvironment().getProperty(key));
                        }
                        if (key.startsWith(WIREMOCK_TEST_PROP2BASEPATH_SECURE_MAP_PREFIX)) {
                            propertiesSecureMap.put(key.substring(WIREMOCK_TEST_PROP2BASEPATH_SECURE_MAP_PREFIX.length()), applicationContext.getEnvironment().getProperty(key));
                        }
                    });

            for (Map.Entry<String, String> entry : propertiesMap.entrySet()) {
                setWireMockBaseMockedServicePath(applicationContext,serverWireMockExtension.getRuntimeInfo().getHttpBaseUrl(),entry);
            }
            for (Map.Entry<String, String> entry : propertiesSecureMap.entrySet()) {
                setWireMockBaseMockedServicePath(applicationContext,serverWireMockExtension.getRuntimeInfo().getHttpsBaseUrl(),entry);
            }

            System.out.printf("""
                            ************************
                            Server wiremock:
                            http base url: %s
                            https base url: %s
                            ************************
                            """,
                    serverWireMockExtension.getRuntimeInfo().getHttpBaseUrl(),
                    serverWireMockExtension.getRuntimeInfo().getHttpsBaseUrl());
        }

        private static void setWireMockBaseMockedServicePath(ConfigurableApplicationContext applicationContext, String serverWireMock,Map.Entry<String, String> entry){
            TestPropertySourceUtils.addInlinedPropertiesToEnvironment(applicationContext,String.format("%s=%s/%s",entry.getKey(),serverWireMock,entry.getValue()));
        }
    }
//endregion
}
