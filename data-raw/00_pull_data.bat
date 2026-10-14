@echo off
REM ==========================================================================
REM  Double-click to download the SARB / FRED inputs for baeconstrat.
REM  Finds Rscript (PATH first, then the standard install folders), runs
REM  data-raw\01_pull_data.R from the package root, and keeps the window open.
REM ==========================================================================
setlocal EnableDelayedExpansion
cd /d "%~dp0.."

set "RSCRIPT="
for /f "delims=" %%i in ('where Rscript 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%i"
if not defined RSCRIPT for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
if not defined RSCRIPT for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
if not defined RSCRIPT for /d %%D in ("%USERPROFILE%\Documents\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"

if not defined RSCRIPT (
  echo Could not find Rscript.exe. Install R or add it to PATH.
  pause
  exit /b 1
)

echo Using "!RSCRIPT!"
echo Working directory: %CD%
echo.
"!RSCRIPT!" --no-save --no-restore "data-raw\01_pull_data.R"
echo.
echo Finished with exit code %ERRORLEVEL%. Log: data-raw\logs\pull_latest.log
echo PULL_FINISHED %DATE% %TIME% > "data-raw\logs\PULL_DONE.txt"
pause
