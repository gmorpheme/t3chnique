statusListener(OnConsoleStatusListener)

println("Configuring default logging from logback.groovy")

appender("FILE", RollingFileAppender) {
  encoder(PatternLayoutEncoder) {
    pattern = "%d{HH:mm:ss.SSS} %-5level %logger{12} - %msg%n"
  }
  rollingPolicy(TimeBasedRollingPolicy) {
    fileNamePattern = "./t3chnique-%d{yyyy-MM-dd_HH}.log"
    maxHistory = 10
    cleanHistoryOnStart = true
  }
}

scan("60 seconds")

logger("t3chnique", TRACE)

root(INFO, ["FILE"])
