source("R/00_setup.R")
packages <- c("lme4", "lmerTest", "broom.mixed", "performance")
absent <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(absent)) stop("Missing R packages: ", paste(absent, collapse = ", "), call. = FALSE)

scores <- read_scores()
trial_data <- list(
  vowel_identification = read_core("identification_trials.csv", id_fields),
  vowel_production = read_core("production_trials.csv", production_fields),
  vowel_imitation = read_core("imitation_trials.csv", imitation_fields)
)
random_terms <- c(
  vowel_identification = "(1 | case_id) + (1 | contrast) + (1 | talker)",
  vowel_production = "(1 | case_id) + (1 | word)",
  vowel_imitation = "(1 | case_id) + (1 | word) + (1 | talker)"
)

for (outcome in outcomes) {
  x <- trial_data[[outcome]]
  response <- if (outcome == "vowel_identification") "correct" else "acoustic_score"
  require_numeric(x, c("trial_index", response))
  if (anyDuplicated(x[c("case_id", "trial_index")]) ||
      any(x$trial_index < 1 | x$trial_index != floor(x$trial_index)) ||
      !setequal(x$case_id, scores$case_id)) {
    stop("Invalid trial keys or unmatched cases in ", outcome, call. = FALSE)
  }
  if (outcome == "vowel_identification" && any(!x$correct %in% c(0, 1))) {
    stop("Identification correct must be coded 0/1.", call. = FALSE)
  }
  for (v in intersect(c("case_id", "contrast", "word", "talker"), names(x))) x[[v]] <- factor(x[[v]])
  if (outcome != "vowel_identification") x$acoustic_score_z <- standardize(x$acoustic_score)
  x[predictors] <- scores[match(as.character(x$case_id), scores$case_id), predictors]
  trial_data[[outcome]] <- x
}

fit_model <- function(outcome, terms, reml = TRUE) {
  response <- if (outcome == "vowel_identification") "correct" else "acoustic_score_z"
  fixed <- if (length(terms)) paste(terms, collapse = " + ") else "1"
  formula <- stats::as.formula(paste(response, "~", fixed, "+", random_terms[[outcome]]))
  if (outcome == "vowel_identification") {
    lme4::glmer(
      formula, data = trial_data[[outcome]], family = stats::binomial,
      control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
  } else {
    lmerTest::lmer(
      formula, data = trial_data[[outcome]], REML = reml,
      control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
  }
}

models <- setNames(lapply(outcomes, function(outcome) fit_model(outcome, predictors)), outcomes)
fixed_effects <- do.call(rbind, lapply(outcomes, function(outcome) {
  tt <- as.data.frame(broom.mixed::tidy(models[[outcome]], effects = "fixed", conf.int = TRUE))
  tt <- tt[tt$term %in% predictors, ]
  tt$outcome <- outcome
  tt$odds_ratio <- if (outcome == "vowel_identification") exp(tt$estimate) else NA_real_
  tt$or_low <- if (outcome == "vowel_identification") exp(tt$conf.low) else NA_real_
  tt$or_high <- if (outcome == "vowel_identification") exp(tt$conf.high) else NA_real_
  if (!"df" %in% names(tt)) tt$df <- NA_real_
  tt[, c("outcome", "term", "estimate", "std.error", "statistic", "df", "p.value", "conf.low", "conf.high", "odds_ratio", "or_low", "or_high")]
}))
fixed_effects$p_fdr <- stats::p.adjust(fixed_effects$p.value, method = "BH")
write_result(fixed_effects, "fixed_effects.csv")

diagnostics <- do.call(rbind, lapply(outcomes, function(outcome) {
  model <- models[[outcome]]
  r2 <- performance::r2_nakagawa(model)
  messages <- model@optinfo$conv$lme4$messages
  data.frame(
    outcome = outcome, n_trials = stats::nobs(model),
    formula = paste(deparse(stats::formula(model)), collapse = " "),
    reml = if (outcome == "vowel_identification") FALSE else lme4::isREML(model),
    singular = lme4::isSingular(model),
    convergence_message = if (is.null(messages)) "" else paste(messages, collapse = " | "),
    marginal_r2 = if (is.null(r2$R2_marginal)) NA_real_ else unname(r2$R2_marginal),
    conditional_r2 = if (is.null(r2$R2_conditional)) NA_real_ else unname(r2$R2_conditional)
  )
}))
write_result(diagnostics, "model_diagnostics.csv")

nested <- do.call(rbind, lapply(outcomes, function(outcome) {
  sequence <- lapply(0:3, function(k) fit_model(outcome, predictors[seq_len(k)], reml = FALSE))
  do.call(rbind, lapply(1:3, function(k) {
    comparison <- stats::anova(sequence[[k]], sequence[[k + 1]], refit = FALSE)
    data.frame(
      outcome = outcome, added_predictor = predictors[[k]],
      chisq = comparison$Chisq[[2]], df = comparison$Df[[2]],
      p = comparison$`Pr(>Chisq)`[[2]],
      aic_small = comparison$AIC[[1]], aic_large = comparison$AIC[[2]],
      delta_aic = comparison$AIC[[1]] - comparison$AIC[[2]]
    )
  }))
}))
write_result(nested, "nested_comparisons.csv")
