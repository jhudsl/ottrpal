test_that("Quiz checks", {
  # Using good quiz md example

  quiz_path <- good_quiz_path()
  good_quiz <- readLines(quiz_path)
  good_quiz_specs <- parse_quiz(good_quiz)
  good_quiz_checks <- check_all_questions(good_quiz_specs)

  # Using bad quiz md example
  bad_quiz <- readLines(bad_quiz_path())
  bad_quiz_specs <- parse_quiz(bad_quiz)

  # THe following checks *should fail* because we're giving it a bad quiz.
  bad_quiz_checks <- suppressWarnings(check_all_questions(bad_quiz_specs))

  ## Make a temporary quiz directory
  quiz_dir <- dirname(good_quiz_path())

  ## Now check the quizzes in that directory
  # The following checks *should also fail* because the bad quiz is in there
  all_quiz_results <- suppressWarnings(check_quizzes(quiz_dir = quiz_dir))

  ## TEST HERE:
  # 1. quiz_error_report.tsv should be made
  # 2. bad quiz should have errors
  # 3. good quiz should pass!

})
