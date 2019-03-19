echo off
set username=#username#
set service=#service#
cls
set /p password="Enter password for user %username%: "
cls
echo on

sqlplus %username%/%password%@%service% @create.sql
sqlldr %username%/%password%@%service% control=loader.ctl skip=1