
#' @import yasp
#' @export
register_all <- function(force = TRUE) {
  stopifnot(is_windows())

  add_open_Rstudio_here_right_click_context_action(force = force)
  add_run_Rscript_right_click_context_action(force = force)
}


#' @export
add_run_Rscript_right_click_context_action <- function(force = FALSE, echo = FALSE) {
  stopifnot(is_windows())

  if (!can_find_R_on_PATH() || force)
    add_R_to_path(force = force, echo = echo)

  pth2launch_script <- system.file("run_rscript.cmd", package = "windows", mustWork = TRUE)
  pth2launch_script <- shQuote(normalizePath(pth2launch_script, mustWork = TRUE))
  reg_key_data <- shQuote(p(pth2launch_script, dbl_quote("%1")))

  key <-
    '"HKEY_CURRENT_USER\\Software\\Classes\\R_auto_file\\shell\\Run Rscript\\command"'

  if (is_reg_key_exists(key) && !force) {
    message("key already exists")
    return(invisible())
  }

  message('Adding "Run Rscript" context action. Right-clicking on an ".R" file should now show "Run Rscript" as an option.')

  add_reg_key(key, data = reg_key_data, force = force, echo = echo)
  invisible()
}


set_R_BIN_user_variable <- function(echo = FALSE) {
  # uses setx
  r_bin <- normalizePath(R.home("bin"))
  if(nchar(r_bin) > 1023)
    stop("Path to R is too long for this interface, which is based on setx.exe and has a character limit of 1024 for any particular variable. Use windows:::add_reg_key() instead")
  set_user_variable("R_BIN", r_bin, echo = echo)
}


#' @export
add_R_to_path <- function(force = FALSE, echo = FALSE) {
  # we query the registry directly because we don't want variables like
  # "%R_BIN%" expanded yet. Note, a user PATH is only expanded once, so you
  # can't nest user variables. e.g. you can't add %R_PATH% and then define
  # %R_PATH% as %R_HOME%\\bin\\%R_ARCH% because %R_PATH% will not be expanded
  # fully (or at all, really). This is different from system varialbes, which
  # are expaned recursively.
  current_user_path <- read_user_var_direct_from_registry("PATH")

  message("\nThe current user PATH Variable:")
  cat(pcnl(p('\t', current_user_path)), "\n\n")

  message("Prepending %R_BIN% to the PATH" )

  new_user_path <- unique(c("%R_BIN%", current_user_path ))
  new_user_path <- p(new_user_path, collapse = ";")

  # escape_percents
  new_user_path <- gsub("%", '^%', new_user_path, fixed = TRUE)

  # spaces need to be quoted (different from most other cmd.exe commands, which
  # require the file path to be quoted)
  new_user_path <- gsub("(\\s+)", '"\\1"', new_user_path, perl = TRUE)

  # We use reg add instead of setx because setx truncates any chars over 1024,
  # also, it doesn't let you easily distinguish user PATH from system PATH.
  # the only reliable way is to query the registry
  add_reg_key(
    "HKCU\\Environment",
    value = "PATH",
    type = "REG_EXPAND_SZ",
    data = new_user_path,
    force = TRUE,
    echo = echo
  )

  message("\nThe user PATH variable is now:")
  cat(pcnl(p('\t', read_user_var_direct_from_registry("PATH"))), "\n\n")

  message("As a reminder, the System PATH is searched before the User PATH")
  message("The System PATH is:")
  cat(pcnl(p('\t', read_system_var_direct_from_registry("PATH"))), "\n\n")

  set_R_HOME_user_variable()
}




#' @export
add_open_Rstudio_here_right_click_context_action <- function(force = FALSE, echo = TRUE) {
  key <- dbl_quote("HKEY_CURRENT_USER\\Software\\Classes\\directory\\Background\\shell\\Open Rstudio here\\command")
  add_reg_key(key, data = "rstudio.exe", force = force, echo = echo)
}

# this is what you would think to do if you wanted to right click on a folder
# and see the option to Open Rstudio here... sadly, this doesn't work, it still
# opens up with the working director in the parent folder.
#  key <- dbl_quote("HKEY_CURRENT_USER\\Software\\Classes\\Folder\\shell\\Open Rstudio here\\command")
#  add_reg_key(key, data = shQuote('rstudio.exe "%1"'), echo = TRUE, force = TRUE)
