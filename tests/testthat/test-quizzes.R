test_that("Check good quiz", {
  # Using good quiz md example

  quiz_path <- good_quiz_path()
  good_quiz <- readLines(quiz_path)
  good_quiz_specs <- parse_quiz(good_quiz)
  good_quiz_checks <- check_all_questions(good_quiz_specs)

  # It should find no errors with the good quiz
  testthat::expect_true(nrow(good_quiz_checks) == 0)
})

test_that("Check bad quiz", {
  # Using bad quiz md example
  bad_quiz <- readLines(bad_quiz_path())
  bad_quiz_specs <- parse_quiz(bad_quiz)

  # The following checks *should fail* because we're giving it a bad quiz.
  bad_quiz_checks <- try(check_all_questions(bad_quiz_specs), silent = TRUE)

  # It should find two errors
  testthat::expect_true(nrow(bad_quiz_checks) == 2)
})

test_that("Make a quiz report", {
  ## Make a temporary quiz directory
  quiz_dir <- dirname(good_quiz_path())

  ## Now check the quizzes in that directory
  # The following checks *should also fail* because the bad quiz is in there
  all_quiz_results <- try(check_quizzes(quiz_dir = quiz_dir), silent = TRUE)

  # Should have a report saved to the quiz directory
  testthat::expect_true(file.exists(file.path(quiz_dir, "question_error_report.tsv")))
})
