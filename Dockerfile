FROM library/java:openjdk-8u45-jdk
MAINTAINER greg@gmorpheme.net

EXPOSE 8080

COPY ./target/t3chnique-0.0.1-SNAPSHOT.jar ./t3chnique-0.0.1-SNAPSHOT.jar
CMD ["java", "-jar", "./t3chnique-0.0.1-SNAPSHOT.jar"]
