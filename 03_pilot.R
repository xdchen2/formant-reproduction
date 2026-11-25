source("R/00_setup.R")
pilot <- read_core("pilot_formant_accuracy.csv", pilot_fields)
require_numeric(pilot, pilot_fields)
if (any(as.matrix(pilot) < 0 | as.matrix(pilot) > 100)) {
  stop("Pilot accuracy must be on the 0-100 scale.", call. = FALSE)
}
if (nrow(pilot) < 2) stop("Pilot confidence intervals require at least two rows.", call. = FALSE)

pilot_summary <- do.call(rbind, lapply(seq_along(pilot_fields), function(i) {
  x <- pilot[[pilot_fields[[i]]]]
  n <- length(x)
  se <- stats::sd(x) / sqrt(n)
  margin <- stats::qt(.975, df = n - 1) * se
  data.frame(
    formant_step_hz = c(50, 100, 150, 200)[[i]], n = n,
    mean_accuracy = mean(x), sd_accuracy = stats::sd(x),
    ci_low = mean(x) - margin, ci_high = mean(x) + margin
  )
}))
write_result(pilot_summary, "pilot_descriptives.csv")
