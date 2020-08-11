# see https://stats.stackexchange.com/a/67450
# for more on this

calc_score <- function(odds,
                       base_odds = 50,
                       base_score = 650,
                       pdo = 15,
                       upper_limit = 1000,
                       lower_limit = 0) {
  
  # scale:
  
  scale_factor <- pdo / log(2)
  
  # intercept:
  
  intercept <- base_score - log(base_odds) * scale_factor
  
  # score:
  
  raw_score <- intercept + scale_factor * log(odds)
  
  # clip and integer the score:
  
  as.integer(pmax(pmin(raw_score, upper_limit), lower_limit))
  
}
