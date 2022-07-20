#' @import dplyr
#' @importFrom stringr str_which str_detect
#' @importFrom purrr map_df
#' @export
get_opinion <- function(doc, n = 1, agg = TRUE) {
  data("polarity")
  
  get_ngram <- function(x, n = 1) {
    morp <- unlist(morpho_mecab(x, type = "morpheme"))
    morp <- morp[stringr::str_which(names(morp), "[^SF]")]
    
    morp <- paste(morp, names(morp), sep = "/")
    
    N <- length(morp)
    term <- character(N - n + 1)
    
    for (i in seq(term)) {
      term[i] <- paste(morp[i:(i+n-1)], collapse = ";")
    }
    
    term
  }
  
  get_polarity <- function(x, n = 1) {
    data.frame(ngram = get_ngram(x, n = n), stringsAsFactors = FALSE) %>%
      dplyr::left_join(polarity) %>%
      dplyr::filter(!is.na(freq)) %>%
      dplyr::filter(!stringr::str_detect(ngram, "^/J")) %>%
      dplyr::filter(!stringr::str_detect(ngram, "^/ETM")) %>%
      dplyr::filter(!stringr::str_detect(ngram, "^/MM")) %>%
      dplyr::summarise(complex = mean(COMP),
                       negative = mean(NEG),
                       positive = mean(POS),
                       neutral = mean(NEUT),
                       none = mean(None),
                       vote = ifelse(n() == 0, "NOMATCH",
                                     names(sort(table(max.value), decreasing = TRUE))[1]),
                       polarity = (positive - negative) / (positive + negative),
                       subjectivity = (positive + negative) /
                         (positive + negative + complex + neutral + none))
  }
  
  result <- doc %>%
    purrr::map_df(get_polarity, n = n) %>%
    dplyr::filter(vote != "NOMATCH")
  
  if (agg) {
    if (nrow(result) > 0) {
      result  %>%
        dplyr::summarise(Complex = mean(complex, na.rm = TRUE),
                         Negative = mean(negative, na.rm = TRUE),
                         Positive = mean(positive, na.rm = TRUE),
                         Neutral = mean(neutral, na.rm = TRUE),
                         None = mean(none, na.rm = TRUE),
                         vote = names(sort(table(vote), decreasing = TRUE))[1],
                         polarity = mean((positive - negative) / (positive + negative)),
                         subjectivity = mean((positive + negative) /
                                               (positive + negative + complex + neutral + none))) %>%
        dplyr::select(complex = Complex,
                      negative = Negative,
                      positive = Positive,
                      neutral = Neutral,
                      none = None,
                      vote,
                      polarity,
                      subjectivity) %>%
        return()
    } else {
      return(result)
    }
  } else {
    data.frame(result, doc = doc) %>%
      return()
  }
}


get_chunk_id <- function(N, chunk) {
  n <- N %/%  chunk
  
  if (N %%  chunk > 0) n <- n + 1
  
  idx_start <- (seq(n) - 1) * chunk + 1
  
  idx_end <- seq(n) * chunk
  idx_end[n] <- N
  
  list(idx_start = idx_start, idx_end = idx_end)
}


