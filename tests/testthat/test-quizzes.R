test_that("Quiz checks", {
  # Using good quiz md example

  quiz_path <- good_quiz_path()
  good_quiz <- readLines(quiz_path)
  good_quiz_specs <- parse_quiz(good_quiz)
  good_quiz_checks <- check_all_questions(good_quiz_specs)

  # Using bad quiz md example

  bad_quiz <- readLines(bad_quiz_path())
  bad_quiz_specs <- parse_quiz(bad_quiz)
  bad_quiz_checks <- check_all_questions(bad_quiz_specs)

  ## Make a temporary quiz directory
  quiz_dir <- dirname(good_quiz_path())

  ## Now check the quizzes in that directory
  all_quiz_results <- check_quizzes(quiz_dir = quiz_dir)

  ottrpal::check_quizzes(quiz_dir = file.path(root_dir, "quizzes"), write_report = TRUE, verbose = TRUE)
})
