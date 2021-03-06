# windows

`windows` is an R package that allows you easily:

+   add R and Rscript to the PATH
+   add a right click context menu action "Run R script", which runs the R script with in a fresh and isolated R session, then gives you some information after it's done running. (see screenshots) 

![](man/figures/run_rscript_menu_screenshot.png) 

Clicking on"Run Rscript" brings up:

![](man/figures/run_rscript_cmd_window_screenshot.png) 

the R file is executed via  `Rscript --vanilla` in a separate isolated session. In this example, the script shown consists solely of `library(tidyverse)`). The timer and other information is added by the context menu.


+   add a right click context menu action "Open Rstudio here" 

![](man/figures/open_rstudio_here_menu_screenshot.png)


Tested on windows 7 and windows 10. 

to install:
``` r
devtools::install_github("t-kalinowski/windows")
```

to use:
``` r
windows::register_all()
```
*  This will add R to the user path
*  add the "Run Rscript" right click context menu for R files
*  add the "Open Rstudio here" right click context for windows explorer windows

Alternatively, if you only want some specific additions, these functions are 
also exported.
``` r
add_R_to_path()
add_run_Rscript_right_click_context_action()
add_open_Rstudio_here_right_click_context_action()
```

