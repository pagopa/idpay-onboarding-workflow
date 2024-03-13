package it.gov.pagopa.common.rest.utils;

import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import com.github.tomakehurst.wiremock.extension.responsetemplating.ResponseTemplateTransformer;
import com.github.tomakehurst.wiremock.junit5.WireMockExtension;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;

public class WireMockUtils {
    private WireMockUtils(){}
    public static WireMockExtension initServerWiremock(int httpPort, int httpsPort, String stubsFolder, boolean needClientAuth, String trustorePath, String trustorePassword) {
        return WireMockExtension.newInstance()
                .options(getWireMockConfiguration(httpPort, httpsPort, stubsFolder, needClientAuth, trustorePath, trustorePassword))
                .build();
    }
    private static WireMockConfiguration getWireMockConfiguration(int httpPort, int httpsPort, String stubsFolder, boolean needClientAuth, String trustorePath, String trustorePassword){
        return wireMockConfig()
                .port(httpPort)
                .httpsPort(httpsPort)
                .needClientAuth(needClientAuth)
                .keystorePath("src/test/resources/wiremockKeyStore.p12")
                .keystorePassword("idpay")
                .keyManagerPassword("idpay")
                .trustStorePath(trustorePath)
                .trustStorePassword(trustorePassword)
                .usingFilesUnderClasspath(stubsFolder)
                .extensions(new ResponseTemplateTransformer.Builder().global(true).build());
    }
}