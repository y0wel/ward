source("~/dev/ward/utils.R")

check_dependencies()

current_directory_uses_git()

t1 <- scan_files()

repeat {
  t2 <- scan_files()
  t2 <- check_file_count()
  relevant_test <- compare_times()

  if (length(relevant_test) == 0) {
    t1 <- t2
    Sys.sleep(2)
    next
  }

  if (startsWith(relevant_test, "etl")) {
    system('clear && printf "\\e[3J"')
    Sys.sleep(1)
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
    Sys.sleep(1)
    test_path <-
      paste0(repo_path, "/", relevant_test)
    message(
      paste(
        paste0("Starting tests at ", Sys.time(), ": ", test_path),
        "",
        sep = "\n"
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
