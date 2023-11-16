read_fiveprimaryJSON <- function(path) {
  result <- read_json(path, simplifyVector = TRUE)$data
  thresholds <- data.frame(preset = result$preset$presetName,
                           frequency = result$preset$modulation$frequencyRed,
                           strategy = ifelse(result$preset$strategy$testContrast,
                                             "contrast", "frequency"),
                           finished = result$finished,
                           threshold = result$value)
  return(thresholds)
}

