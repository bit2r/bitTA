#' KOSAC(Korean Sentiment Analysis Corpus) Sentiment Analysis
#' @description 한국어 감정분석 코퍼스를 활용하여 문서의 감성분석 결과를 반환
#' @param doc	character. KOSAC를 이용해서 감성분석을 수행할 문자열 벡터
#' @param n	integer. n-gram 토큰화 계수
#' @param agg	logical. 집계 여부. TRUE이면 집계 결과만  반환하며, FALSE이면 문자열벡터도 함께 반환
#' @return data.frame 감성분석 결과를 담은 data.frame
#' \itemize{
#'   \item complex: numeric. 복합
#'   \item negative: numeric. 부정
#'   \item positive: numeric. 긍정
#'   \item neutral: numeric. 중립
#'   \item none: numeric. 해당없음
#'   \item vote: character. 감성 투표 결과. "POS", "NEG"
#'   \item polarity: numeric. 극성
#'   \item subjectivity: numeric. 주관성 (부정+긍정의 합)
#'   \item doc: character. 문서의 내용
#' }
#' @examples
#' \donttest{
#' get_opinion(buzz$CONTENT[1])
#' }
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
      dplyr::left_join(polarity, by = "ngram") %>%
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
  chunk <- ifelse(chunk == 0, 1, chunk)
  
  n <- N %/%  chunk
  
  if (N %%  chunk > 0) n <- n + 1
  
  idx_start <- (seq(n) - 1) * chunk + 1
  
  idx_end <- seq(n) * chunk
  idx_end[n] <- N
  
  list(idx_start = idx_start, idx_end = idx_end)
}


