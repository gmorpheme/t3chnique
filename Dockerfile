FROM library/java:openjdk-8u45-jdk
MAINTAINER greg@gmorpheme.net

ENV T3CHNIQUE_VERSION 0.0.1-SNAPSHOT
EXPOSE 8080

COPY ./target /opt/t3chnique
CMD java -jar /opt/t3chnique/t3chnique-$T3CHNIQUE_VERSION.jar
