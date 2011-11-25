@echo off
set SCRIPT_DIR=%~dp0
chcp 1253
java %SBT_OPTS% -Dfile.encoding=Cp1253 -Xss4M -Xmx1024M -XX:MaxPermSize=256M -XX:NewSize=128M -XX:NewRatio=3 -jar "%SCRIPT_DIR%sbt-launch.jar" %*
chcp 437
