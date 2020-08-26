@echo off
echo Starter...
rscript.exe -e "library(fmtsafeconlog); library(RDCOMClient); fmtsafeconlog('Safecon','example.yaml')"
echo Ferdig.
pause
