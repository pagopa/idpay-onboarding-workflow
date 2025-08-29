package it.gov.pagopa.onboarding.workflow.enums;

import lombok.Getter;

@Getter
public enum ChannelType {
    APP_IO("APP_IO"),
    WEB("WEB");

    private final String description;

    ChannelType(String description) {
        this.description = description;
    }
}
