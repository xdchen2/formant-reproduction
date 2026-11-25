source("R/00_setup.R")
scores <- read_scores()
variables <- c(predictors, outcome_vars)

descriptives <- do.call(rbind, lapply(variables, function(v) {
  data.frame(variable = v, n = nrow(scores), mean = mean(scores[[v]]), sd = stats::sd(scores[[v]]))
}))

correlations <- do.call(rbind, lapply(utils::combn(variables, 2, simplify = FALSE), function(pair) {
  test <- stats::cor.test(scores[[pair[[1]]]], scores[[pair[[2]]]], method = "pearson")
  data.frame(
    x = pair[[1]], y = pair[[2]], n = nrow(scores),
    r = unname(test$estimate), p = test$p.value,
    conf_low = test$conf.int[[1]], conf_high = test$conf.int[[2]]
  )
}))

equivalence <- correlations[correlations$x == "melody_reproduction_z" & correlations$y %in% outcome_vars, ]
equivalence$equivalence_bound <- 0.20
equivalence$ci_level <- 0.95
equivalence$ci_within_bound <- equivalence$conf_low > -0.20 & equivalence$conf_high < 0.20

write_result(descriptives, "composite_descriptives.csv")
write_result(correlations, "correlations.csv")
write_result(equivalence, "pitch_equivalence.csv")
