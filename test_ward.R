library(testthat)

temp_dir <- paste0("~/ward_test_dir")
if (dir.exists(temp_dir)) {
  system(paste("rm -r", temp_dir))
}

context("current_directory_uses_git()")
describe("current_directory_uses_git()", {
  dir.create(temp_dir)

  suppressMessages(
    system(paste("cd", temp_dir, "; git init"), intern = TRUE)
  )
  expected_result <-
    paste0(dirname(temp_dir), "/", gsub("~/", "", temp_dir))

  it("returns the correct directory path", {
    expect_equal(
      current_directory_uses_git(dir = paste0("cd ", temp_dir, ";")),
      expected_result
    )
  })

  system(
    paste("cd", temp_dir, "; rm -rf .git; cd; rm -r", temp_dir)
  )
})

context("scan_files()")
describe("scan_files()", {
  dir.create(temp_dir)

  it("returns a warning", {
    expect_warning(
      scan_files(),
      "No test files found!"
    )
  })

  file <- "test.R"
  setwd(repo_path)
  file.create(file)

  scanned_files <- scan_files()

  it("returns file info", {
    expect_equal(
      nrow(scanned_files), 1
    )
    expect_match(
      scanned_files$file,
      file
    )
  })
  
  new_file <- "test.txt"
  file.create(new_file)

  scanned_files <- scan_files()

  it("returns file info only of files of allowed file type", {
    expect_equal(
      nrow(scanned_files), 1
    )
    expect_match(
      scanned_files$file,
      file
    )
  })
})

context("check_file_count()")
describe("check_file_count()", {
  files_t1 <- scan_files()
  files_t2 <- files_t1

  it("returns files_t1", {
    expect_equal(
      check_file_count(files_at_t1 = files_t1, files_at_t2 = files_t2),
      files_t1
    )
  })

  file <- "test2.R"
  file.create(file)
  files_t2 <- scan_files()

  it("returns files_t2", {
    expect_equal(
      check_file_count(files_at_t1 = files_t1, files_at_t2 = files_t2),
      files_t2
    )
  })
})

context("compare_times()")
describe("compare_times()", {
  it("returns the last updated file 'test2.R'", {
    expect_equal(
      compare_times(times_1 = files_t1, times_2 = files_t2),
      files_t2$file[which(files_t2$timestamps == max(files_t2$timestamps))]
    )
  })

  file <- "test3.R"
  file.create(file)
  files_t2 <- scan_files()

  it("returns the last updated file 'test3.R'", {
    expect_equal(
      compare_times(times_1 = files_t1, times_2 = files_t2),
      files_t2$file[which(files_t2$timestamps == max(files_t2$timestamps))]
    )
  })

  system(
    paste("cd", temp_dir, "; cd; rm -r", temp_dir)
  )
})
