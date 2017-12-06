@echo on
setlocal
title rendering %~nx1
echo:
  echo Rendering R markdown file: %~nx1
echo      in directory: %~dp1
echo:
  echo ---- Start of Console Output -------
  set startdate=%date%
  set starttime=%time%
  Rscript --vanilla -e "rmarkdown::render(\"%~nx1\")"
set endtime=%time%
  set enddate=%date%
  echo:------ End of Script Output -------
  echo:
  echo:
  echo Finished running R script:
  echo:       %~nx1
echo in directory:
  echo:       %~dp1
echo:
  echo:
  set expr="{ar<-commandArgs(TRUE); dt <- function(x) strptime(paste(x, collapse = ' '), format = '%%m/%%d/%%Y %%H:%%M:%%OS'); f <- function(x) format(x, '%%F %%r'); start <- dt(ar[2:3]); end <- dt(ar[5:6]); runtime <- end - start; cat('The R script:', dQuote(ar[7]), '\n\t started at: ', f(start),'\n\tfinished at: ', f(end), '\n\t    ran for: ', format(runtime, digits = 2), '\n')}"

Rscript --vanilla --default-packages=base -e %expr% %startdate% %starttime% %enddate% %endtime% "%~nx1"
echo:
  PAUSE


:: HELP CALL
:: HELP SET
