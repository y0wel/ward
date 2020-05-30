uses_git <-
  suppressWarnings(
    system("[ -d .git ] && echo .git || git rev-parse --git-dir > /dev/null 2>&1", intern = TRUE)
  )

if (length(uses_git) == 0) {
  message("The current directory is not a git repository! Shutting down ...")
  Sys.sleep(2)
  quit(save = "no")
}
  
repo_path <-
  system("git rev-parse --show-toplevel", intern = TRUE)

repo_path <-
  "/Users/tobiasmatz/Desktop/compute-r"

scan_files <- function(path = repo_path) {
  file <-
    list.files(path, recursive = TRUE)
  if (length(file) == 0) {
    warning("No test files found!")
    return(NULL)
  }
  timestamps <-
    unlist(
      lapply(
        list.files(path, recursive = TRUE),
        function(x)
          as.character(
            file.info(
              paste0(path, "/", x)
            )$mtime
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

t1 <-
  scan_files()

while(TRUE) {
  t2 <-
    scan_files()
  t2 <-
    check_file_count()
  relevant_test <-
    compare_times()
  if (length(relevant_test) == 0) {
    t1 <- t2
    Sys.sleep(2)
    next
  }
  if (startsWith(relevant_test, "etl")) {
    system('clear && printf "\\e[3J"')
    splitted_file_path <-
      unlist(strsplit(relevant_test, split = "/"))
    file_name <-
      splitted_file_path[length(splitted_file_path)]
    file_path <-
      paste(splitted_file_path[-length(splitted_file_path)], collapse = "/")
    test_path <-
      paste0(repo_path, "/", gsub("etl", "test", file_path), "/test_", file_name)
    if (file.exists(test_path)) {
      message(
        paste(
          paste0("Starting tests at ", Sys.time(), ": ", test_path),
          "",
          sep =  "\n"
        )
      )
      testthat::test_file(test_path)
      t1 <- t2
      Sys.sleep(2)
      next
    } else {
      message("There is no corresponding test file!")
      t1 <- t2
      Sys.sleep(2)
      next
    }
  }
  if (startsWith(relevant_test, "test")) {
    system('clear && printf "\\e[3J"')
    test_path <-
      paste0(repo_path, "/", relevant_test)
    message(
      paste(
        paste0("Starting tests at ", Sys.time(), ": ", test_path),
        "",
        sep =  "\n"
      )
    )
    testthat::test_file(test_path)
    t1 <- t2
    Sys.sleep(2)
    next
  }
  t1 <- t2
  Sys.sleep(2)
}
