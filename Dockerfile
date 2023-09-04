#
# Build
#
FROM maven:3.9.4-amazoncorretto-17-al2023@sha256:c668a2ee8a376c82977408f57970a996f5d6d3d5f017149d02d396eed2c850b3 AS buildtime

WORKDIR /build
COPY . .

RUN mvn clean package -DskipTests

#
# Docker RUNTIME
#
FROM amazoncorretto:17.0.8-alpine3.18@sha256:f59b4f511346db4e473fb98c65b86254926061ce2b398295e975d0632fa4e2fd AS runtime

RUN apk add shadow
RUN useradd --uid 10000 runner

VOLUME /tmp
WORKDIR /app

COPY --from=buildtime /build/target/*.jar /app/app.jar
# The agent is enabled at runtime via JAVA_TOOL_OPTIONS.
ADD https://github.com/microsoft/ApplicationInsights-Java/releases/download/3.4.16/applicationinsights-agent-3.4.16.jar /app/applicationinsights-agent.jar

RUN chown -R runner:runner /app

USER 10000

ENTRYPOINT ["java","-jar","/app/app.jar"]
