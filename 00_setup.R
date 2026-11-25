project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
dir.create(file.path(project_root, "results"), showWarnings = FALSE)

predictors <- c(
  "formant_discrimination_z",
  "formant_reproduction_200hz_z",
  "melody_reproduction_z"
)
outcomes <- c("vowel_identification", "vowel_production", "vowel_imitation")
outcome_vars <- paste0(outcomes, "_z")
score_fields <- c("case_id", predictors, outcome_vars)
id_fields <- c("case_id", "trial_index", "contrast", "talker", "correct")
production_fields <- c("case_id", "trial_index", "word", "acoustic_score")
imitation_fields <- c("case_id", "trial_index", "word", "talker", "acoustic_score")
pilot_fields <- paste0("formant_", c(50, 100, 150, 200), "hz_accuracy")

read_core <- function(filename, fields) {
  path <- file.path(project_root, "data", filename)
  if (!file.exists(path)) stop("Missing observed input: ", filename, call. = FALSE)
  x <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (anyDuplicated(names(x)) || !setequal(names(x), fields)) {
    stop("Unexpected or missing columns in ", filename, call. = FALSE)
  }
  x <- x[, fields, drop = FALSE]
  if (!nrow(x) || anyNA(x) || any(vapply(x, function(v) any(trimws(as.character(v)) == ""), logical(1)))) {
    stop("Empty or incomplete input: ", filename, call. = FALSE)
  }
  if ("case_id" %in% fields && any(!grepl("^A[0-9]{3,}$", x$case_id))) {
    stop("case_id must use anonymous codes such as A001.", call. = FALSE)
  }
  x
}

require_numeric <- function(x, fields) {
  if (any(!vapply(x[, fields, drop = FALSE], function(v) is.numeric(v) && all(is.finite(v)), logical(1)))) {
    stop("Required numeric fields contain invalid values.", call. = FALSE)
  }
}

standardize <- function(x) {
  if (!is.numeric(x) || any(!is.finite(x)) || stats::sd(x) == 0) {
    stop("Cannot standardize invalid or constant scores.", call. = FALSE)
  }
  as.numeric(scale(x))
}

read_scores <- function() {
  x <- read_core("analysis_scores.csv", score_fields)
  require_numeric(x, c(predictors, outcome_vars))
  if (anyDuplicated(x$case_id) || nrow(x) < 4) {
    stop("analysis_scores.csv requires unique case_id values and at least four rows.", call. = FALSE)
  }
  for (v in c(predictors, outcome_vars)) {
    if (abs(mean(x[[v]])) > 1e-4 || abs(stats::sd(x[[v]]) - 1) > 1e-4) {
      stop(v, " must be standardized across cases before analysis.", call. = FALSE)
    }
  }
  x
}

write_result <- function(x, filename) {
  utils::write.csv(x, file.path(project_root, "results", filename), row.names = FALSE, na = "")
}
