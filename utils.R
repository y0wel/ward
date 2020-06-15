check_dependencies <- function() {
  dependencies <- c("testthat", "rstudioapi")
  condition <- !dependencies %in% installed.packages()
  if (any(condition)) {
    install.packages(dependencies[which(condition)])
  }
}

current_directory_uses_git <- function(dir = NULL) {
  uses_git <-
    suppressWarnings(
      system(
        paste0(dir, " [ -d .git ] && echo .git || git rev-parse --git-dir > /dev/null 2>&1"),
        intern = TRUE
      )
    )
  if (length(uses_git) == 0) {
    message("The current directory is not a git repository! Shutting down ...")
    Sys.sleep(2)
    quit(save = "no")
  } else {
    repo_path <<-
      system(
        paste0(dir, "git rev-parse --show-toplevel"),
        intern = TRUE
      )
  }
}

scan_files <- function(path = repo_path, allowed_file_type = ".R") {
  file <-
    list.files(path, recursive = TRUE)
  file <-
    file[which(endsWith(file, allowed_file_type))]
  if (length(file) == 0) {
    warning("No test files found!")
    return(NULL)
  }
  timestamps <-
    unlist(
      lapply(
        file,
        function(x)
          as.character(
            file.mtime(
              paste0(path, "/", x)
            )
          )
      )
    )
  output <-
    as.data.frame(
      cbind(file, timestamps),
      stringsAsFactors = FALSE
    )
  return(output)
}

check_file_count <- function(files_at_t1 = t1, files_at_t2 = t2) {
  files_at_t1 <-
    files_at_t2[
      match(
        files_at_t1$file,
        files_at_t2$file
      )[which(!is.na(match(files_at_t1$file, files_at_t2$file)))],
      ]
  return(files_at_t2)
}

compare_times <- function(times_1 = t1, times_2 = t2) {
  changed_files <-
    suppressWarnings(
      which((times_1$timestamps == times_2$timestamps) == FALSE)
    )
  if (length(changed_files) == 0) {
    return(character(0))
  }
  last_changed_file <-
    max(times_2$timestamps[changed_files])
  times_2$file[which(times_2$timestamps == last_changed_file)]
}
