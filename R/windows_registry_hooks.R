

is_windows <- function() {
    .Platform$OS.type == "windows"
}


is_shell_returned_error <- function(shell_res) {
  status <- attr(shell_res, "status")
  !is.null(status) && identical(status, 1L)
}

is_reg_key_exists <- function(key) {
  cmd <- p("reg query", key)
  out <- suppressWarnings(shell(cmd, intern = TRUE))
  status <- attr(out, "status")
  is.null(status) && !identical(status, 1L)
}

add_reg_key <- function(key, value, data,
                        type = c("REG_SZ", "REG_EXPAND_SZ", "REG_NONE"),
                        force = FALSE, echo = TRUE) {
  # "REG_DWORD", "REG_QWORD", "REG_BINARY", "REG_MULTI_SZ",
  type <- match.arg(type)
  cmd <- p('reg add', key)

  if(!missing(value))
    cmd <- p(cmd, '/v', value)

  cmd <- p(cmd, '/t', type, '/d', data)

  if (force)
    cmd <- p(cmd, '/f')

  if (is_reg_key_exists(key) && !force) {
    cat("The key is already set to a value. set force = TRUE to overwrite\n")
    return(invisible())
  }

  if (echo) {
    cat("Running the following with cmd.exe:\n")
    cat(cmd, "\n")
  }

  shell(cmd)
}



read_user_var_direct_from_registry <- function(var) {
    cmd <- paste("reg query HKCU\\Environment /v", var)
    x <- suppressWarnings(shell(cmd, intern = T))
    if(is_shell_returned_error(x))
      return("")
    x <- strsplit(x[3], "    ", fixed = TRUE)[[1]]
    x <- strsplit(x[4], ";", fixed = TRUE)[[1]]
    x
}


read_system_var_direct_from_registry <- function(var) {
  cmd <- paste(
    'reg query "HKLM\\SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment" /v',
    var
  )

  x <- suppressWarnings(shell(cmd, intern = T))
  if (is_shell_returned_error(x))
    return("")
  x <- strsplit(x[3], "    ", fixed = TRUE)[[1]]
  x <- strsplit(x[4], ";", fixed = TRUE)[[1]]
  x
}


can_find_R_on_PATH <- function() {
  # make sure that the Rscript can be found on the path and that it is the same
  # version of R as this running session
  Rscript_on_path_info <- suppressWarnings(shell("Rscript --vanilla -e R.Version()", intern = TRUE))
  identical(Rscript_on_path_info, capture.output(print(R.Version())))
}




#' @export
add_run_R_script_right_click_context_action <- function(force = FALSE, echo = FALSE) {
  stopifnot(is_windows())

  if (!can_find_R_on_PATH() || force)
    add_R_to_path(force = force, echo = echo)

  pth2launch_script <- system.file("windows/run_rscript.cmd", package = "TKutils")
  pth2launch_script <- shQuote(normalizePath(pth2launch_script))
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



set_user_variable <- function(name, value, scope = c("user", "system"), echo= FALSE) {
    stopifnot(is_windows())
    scope <- match.arg(scope)

    stopifnot(length(value) == 1, is.character(value),
              nchar(value) < 400) # 1024 I think

    message(p0( "Permenantly Setting User Environment Variable\n",
      name, " to ", value ))

    cmd <- paste("setx", name, shQuote(value))

    # if you use the /M flag in setx, then variables are written to
    # HKEY_LOCAL_MACHINE.  Otherwise, the default is to write to
    # HKEY_CURRENT_USER.
    if (match.arg(scope) == "system")
      cmd <- p(cmd, "/M")

    if(echo)
      cat("Running command:\n", cmd)

    shell(cmd)
}


set_R_Home_user_variable <- function(echo = FALSE) {
  set_user_variable("R_Home", normalizePath(Sys.getenv("R_Home")), echo = echo)
}

set_R_ARCH_user_variable <- function(echo = FALSE) {
  R_ARCH <- .Platform$r_arch
  stopifnot(nzchar(R_ARCH) && R_ARCH %in% c("x64", "i386"))
  set_user_variable("R_ARCH", R_ARCH, echo = echo)
}


#' @export
add_R_to_path <- function(force = FALSE, echo = FALSE) {

  if(can_find_R_on_PATH() && !force) {
    message("R is already on the PATH")
    return(invisible())
  }

  set_R_Home_user_variable(echo = echo)
  if (can_find_R_on_PATH() && !force) {
    message("R is already on the PATH")
    return(invisible())
  }

  set_R_ARCH_user_variable(echo = echo)
  if (can_find_R_on_PATH() && !force) {
    message("R is already on the PATH")
    return(invisible())
  }


  # we query the registry directly because we don't want variables like
  # "%R_Home" expanded yet. Note, a user PATH is only expanded once, so you
  # can't nest user variables. e.g. you can't add %R_PATH% and then define
  # %R_PATH% as %R_Home%\\bin\\%R_ARCH% because %R_PATH% will not be expanded
  # fully (or at all, really). This is different from system varialbes, which
  # are expaned recursively.
  current_user_path <- read_user_var_direct_from_registry("PATH")

  message("\nThe current user PATH Variable:")
  cat(pcnl(p('\t', current_user_path)), "\n\n")

  message("Prepending %R_Home%\\bin\\%R_ARCH% to the PATH" )

  new_user_path <- unique(c("%R_Home%\\bin\\%R_ARCH%", current_user_path ))
  new_user_path <- p(new_user_path, collapse = ";")

  # escape_percents
  new_user_path <- gsub("%", '^%', new_user_path, fixed = TRUE)

  # spaces need to be quoted
  new_user_path <- gsub("(\\s+)", '"\\1"', new_user_path, perl = TRUE)


  # cat(new_user_path)

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
#       "%R_Home%\\bin\\%R_ARCH%",
#       "C:\\Program Files\\RStudio\\bin\\pandoc",
#       "C:\\Python27\\ArcGISx6410.4",
#       "C:\\Python27\\ArcGISx6410.4\\Scripts",
#       "C:\\Python27\\ArcGISx6410.4\\libs",
#       "C:\\Program Files\\ffmpeg-3.2-win64-static\\bin",
#       "C:\\Program Files\\gs\\gs9.21\\bin",
#       "C:\\Users\\kalinowskit\\programs",
#       "C:\\Program Files\\Microsoft VS Code\\bin",
#       "C:\\Program Files\\Inkscape",
#       "C:\\Program Files\\ImageMagick-7.0.3-Q16"
#     )
#
#   if("%R_Home%\\bin\\%R_ARCH%" %in% current_user_path) {
#     message("R is already on the PATH")
#     return(invisible())
#   }



#' @import yasp
#' @export
add_open_Rstudio_here_right_click_context_action <- function(echo = TRUE) {
  key <- dbl_quote("HKEY_CURRENT_USER\\Software\\Classes\\directory\\Background\\shell\\Open Rstudio here\\command")
  add_reg_key(key, data = "rstudio.exe", echo = echo)
}


    # this is if you wanted to right click on a folder and see the option to
    # Open Rstudio here... sadly, this doesn't work, it still opens up with the
    # working director in the parent folder.
#   key <- dbl_quote("HKEY_CURRENT_USER\\Software\\Classes\\Folder\\shell\\Open Rstudio here\\command")
#   add_reg_key(key, data = shQuote('rstudio.exe "%1"'), echo = TRUE, force = TRUE)

