
run_RSetReg.exe <- function(unset = FALSE, type =  c("both", "user", "system")) {
  type <- match.arg(type)
  cmd <- p0(R.home("bin"), "\\RSetReg.exe")
  if(unset)
    cmd <- p(cmd, "/U")

  message("Running cmd:", cmd)

  sys_cmd <- cmd
  usr_cmd <- p(cmd, "/Personal")

  if (type %in% c("user", "both"))
    shell(usr_cmd)

  if (type %in% c("system", "both"))
    shell(sys_cmd)

}


# defunct in favor of R_BIN
set_R_HOME_user_variable <- function(echo = FALSE) {
  set_user_variable("R_HOME", normalizePath(Sys.getenv("R_HOME")), echo = echo)
}
# defunct in favor of R_BIN
set_R_ARCH_user_variable <- function(echo = FALSE) {
  R_ARCH <- .Platform$r_arch
  stopifnot(nzchar(R_ARCH) && R_ARCH %in% c("x64", "i386"))
  set_user_variable("R_ARCH", R_ARCH, echo = echo)
}


collapse <- function(x, collapse) paste(x, collapse = collapse)

## previous version
add_R_to_path <- function(force = FALSE, echo = FALSE) {

  if(can_find_R_on_PATH() && !force) {
    message("R is already on the PATH")
    return(invisible())
  }

  set_R_HOME_user_variable(echo = echo)
  if (can_find_R_on_PATH() && !force) {
    message("R is already on the PATH")
    return(invisible())
  }

  set_R_ARCH_user_variable(echo = echo)
  if (can_find_R_on_PATH() && !force) {
    message("R is already on the PATH")
    return(invisible())
  }

  set_R_BIN_user_variable(echo = echo)
  if (can_find_R_on_PATH() && !force) {
    message("R is already on the PATH")
    return(invisible())
  }

  # we query the registry directly because we don't want variables like
  # "%R_HOME" expanded yet. Note, a user PATH is only expanded once, so you
  # can't nest user variables. e.g. you can't add %R_PATH% and then define
  # %R_PATH% as %R_HOME%\\bin\\%R_ARCH% because %R_PATH% will not be expanded
  # fully (or at all, really). This is different from system varialbes, which
  # are expaned recursively.
  current_user_path <- read_user_var_direct_from_registry("PATH")

  message("\nThe current user PATH Variable:")
  cat(pcnl(p('\t', current_user_path)), "\n\n")

  message("Prepending %R_BIN% to the PATH" )
  new_user_path <- unique(c("%R_BIN%", current_user_path ))

  new_user_path <- collapse(new_user_path, ";")

  # escape_percents
  new_user_path <- gsub("%", '^%', new_user_path, fixed = TRUE)

  # spaces need to be quoted
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

}



#   current_user_path <-
#     c(
#       "%R_HOME%\\bin\\%R_ARCH%",
#       "C:\\Program Files\\RStudio\\bin\\pandoc",
#       "C:\\Python27\\ArcGISx6410.4",
#       "C:\\Python27\\ArcGISx6410.4\\Scripts",
#       "C:\\Python27\\ArcGISx6410.4\\libs",
#       "C:\\Program Files\\ffmpeg-3.2-win64-static\\bin",
#       "C:\\Program Files\\gs\\gs9.21\\bin",
#       "C:\\Program Files\\Microsoft VS Code\\bin",
#       "C:\\Program Files\\Inkscape",
#       "C:\\Program Files\\ImageMagick-7.0.3-Q16"
#     )
#



