statusListener(OnConsoleStatusListener)

println("Configuring default logging from logback.groovy")

appender("CONSOLE", ConsoleAppender) {
  encoder(PatternLayoutEncoder) {
    pattern = "%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n"
  }
}

scan("60 seconds")

logger("t3chnique", WARN)

root(WARN, ["CONSOLE"])