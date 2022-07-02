#' 군산대학교(KNU) 한국어 감성사전
#' 
#' @description 
#' 2018년도 군산대학교 소프트웨어융합공학과 Data Intelligence Lab에서 개발한 
#' 한국어 감성사전으로 총 14,844개의 1-gram, 2-gram, 관용구, 문형, 축약어, 
#' 이모티콘 등에 대한 긍정, 중립, 부정 판별 및 정도(degree)값 계산
#' 
#' @format 4개의 변수와 14,844개의 관측치로 구성된 데이터 프레임.:
#' \describe{
#'   \item{word}{character. 사전 단어}
#'   \item{word_root}{character. 어근}
#'   \item{polarity}{integer. 긍부정의 정보. 매우 부정(-2), 부정(-1), 중립(0), 긍정(1), 매우 긍정(2)}
#'   \item{n_gram}{integer. n-Gram 수}
#' }
#' @docType data
#' @keywords datasets
#' @name sentiment_dic
#' @usage data(sentiment_dic)
#' @source 
#' "KNU 한국어 감성사전" in github <https://github.com/park1200656/KnuSentiLex>
#' @examples
#' \dontrun{
#' data(sentiment_dic)
#' 
#' head(sentiment_dic)
#' }
NULL

# library(dplyr)
# 
# path <- here::here("data", "KnuSentiLex-master", "data")
# fname_sentiword <- glue::glue("{path}/SentiWord_info.json")
# 
# sentiment_dic <- jsonlite::fromJSON(fname_sentiword) %>%
#   mutate(n_gram = stringr::str_count(word, pattern = "\\s+") + 1L) %>% 
#   mutate(polarity = as.integer(polarity))
# 
# save(sentiment_dic, file = glue::glue("data/sentiment_dic.rda"))


#' @import dplyr
#' @importFrom stringr str_which str_detect str_extract
#' @importFrom purrr map_df
#' @export
get_polarity <- function(doc, n = 1) {
  data("sentiment_dic")
  
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
    data.frame(morpheme = get_ngram(x, n = n), stringsAsFactors = FALSE) %>%
      dplyr::mutate(word = stringr::str_extract(morpheme, "[가-힣]+")) %>% 
      dplyr::left_join(sentiment_dic,
                       by = "word") %>%
      dplyr::filter(!is.na(polarity)) %>%
      dplyr::filter(!stringr::str_detect(morpheme, "^/J")) %>%
      dplyr::filter(!stringr::str_detect(morpheme, "^/ETM")) %>%
      dplyr::filter(!stringr::str_detect(morpheme, "^/MM")) %>%
      dplyr::summarise(
        n_match = n(),
        n_negative = sum(polarity < 0),
        n_positive = sum(polarity > 0),
        n_neutral  = sum(polarity == 0),
        negative   = sum(ifelse(polarity < 0, abs(polarity), 0)),
        positive   = sum(ifelse(polarity > 0, polarity, 0)),
        polarity   = (positive - negative) / (positive + negative)
      )
  }
  
  result <- doc %>%
    purrr::map_df(get_polarity, n = n) 
  
  result
}


