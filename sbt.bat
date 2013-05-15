set SCRIPT_DIR=%~dp0
java -Dfile.encoding=UTF8 -Dsbt.boot.directory=.\sbtboot -Dsbt.ivy.home=.\ivyrepo -Xmx512M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -jar "%SCRIPT_DIR%sbt-launch.jar" %*
