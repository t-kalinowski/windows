

add_pandoc_to_path <- function(pth_to_pandoc.exe = Sys.getenv("RSTUDIO_PANDOC"), echo = FALSE) {
  pandoc <- normalizePath(pth_to_pandoc.exe, mustWork = TRUE)
  current_user_path <- read_user_var_direct_from_registry("PATH")
  new_PATH <- unique(c(current_user_path, pandoc))
  new_user_path <- new_PATH

  new_user_path <- p(new_user_path, collapse = ";")

  # escape_percents
  new_user_path <- gsub("%", '^%', new_user_path, fixed = TRUE)

  # spaces need to be quoted
  new_user_path <- gsub("(\\s+)", '"\\1"', new_user_path, perl = TRUE)


  # cat(new_user_path)

  # We use reg add instead of setx because setx truncates any chars over 1024,
  # also, it doesn't let you easily distinguish user PATH from system PATH.
  # the only reliable way is to query the registry
  add_reg_key("HKCU\\Environment", value = "PATH",
              type = "REG_EXPAND_SZ", data = new_user_path,
              force = TRUE, echo = echo
  )

  message("\nThe user PATH variable is now:")
  cat(pcnl(p('\t', read_user_var_direct_from_registry("PATH"))), "\n\n")

  message("As a reminder, the System PATH is searched before the User PATH")
  message("The System PATH is:")
  cat(pcnl(p('\t', read_system_var_direct_from_registry("PATH"))), "\n\n")


  # we need to call setx in order for a WM_SETTINGCHANGE to be broadcast.
  # doesn't matter with what, so just reset R_home
  # http://www.dowdandassociates.com/blog/content/howto-set-an-environment-variable-in-windows-command-line-and-registry/
  set_R_HOME_user_variable()
}
