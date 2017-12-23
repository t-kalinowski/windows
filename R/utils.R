is_windows <- function() {
  .Platform$OS.type == "windows"
}

can_find_R_on_PATH <- function() {
  # make sure that the Rscript can be found on the path and that it is the same
  # version of R as this running session
  Rscript_on_path_info <- suppressWarnings(shell("Rscript --vanilla -e R.Version()", intern = TRUE))
  identical(Rscript_on_path_info, capture.output(print(R.Version())))
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

# uses setx
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


