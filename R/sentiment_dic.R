#' KNU Korean Sentiment Dictionary
#' 
#' @description 
#' 2018년도 군산대학교 소프트웨어융합공학과 Data Intelligence Lab에서 개발한 
#' 한국어 감성사전으로 총 14,841개의 1-gram, 2-gram, ..., 8-gram, 관용구, 문형, 축약어, 
#' 이모티콘 등에 대한 긍정, 중립, 부정 판별 및 정도(degree)값 계산
#' 
#' @format 4개의 변수와 14,841개의 관측치로 구성된 데이터 프레임.:
#' \describe{
#'   \item{word}{character. 사전 단어}
#'   \item{word_root}{character. 어근}
#'   \item{polarity}{integer. 긍부정의 정보. 매우 부정(-2), 부정(-1), 중립(0), 긍정(1), 매우 긍정(2)}
#'   \item{n_gram}{integer. n-Gram 수}
#' }
#' @details 
#' 표준국어대사전을 구성하는 각 단어의 뜻풀이를 분석하여 긍부정어를 추출하였으며, 표준국어대사전을 구성하는 형용사, 부사, 동사, 명사의 모든 뜻풀이에 대한 긍정, 중립, 부정으로 분류하기 위해 Bi-LSTM 딥 러닝 모델 사용
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
# fname_sentiword <- here::here("inst", "data", "KnuSentiLex", "SentiWord_info.json")
# 
# sentiment_dic <- jsonlite::fromJSON(fname_sentiword) %>%
#   filter(!duplicated(word)) %>%
#   mutate(n_gram = stringr::str_count(word, pattern = "\\s+") + 1L) %>%
#   mutate(polarity = as.integer(polarity)) %>%
#   tibble::as_tibble()
# 
# save(sentiment_dic, file = glue::glue("data/sentiment_dic.rda"), version = 2)


#' KNU Korean Sentiment Dictionary Sentiment Analysis
#' @description 군산대학교 한국어 감성 사전을 활용하여 문서의 감성분석 결과를 반환
#' @param doc	character. 군산대학교 한국어 감성 사전을 이용해서 감성분석을 수행할 문자열 벡터
#' @param n	integer. n-gram 토큰화 계수
#' @param indiv logical. 복수개의 문서일 때 개별 문서의 결과를 반환할 지를 선택함.
#' TRUE이면 데이터프레임에서 개별 문서의 결과를 관측치(observations)로 반환하고, FALSE이면 하나의 관측치로 반환함.
#' 기본값은 TRUE
#' @return data.frame 감성분석 결과를 담은 data.frame
#' \itemize{
#'   \item n_match: numeric. 감성사전에 매치된 토큰 개수
#'   \item n_negative: numeric. 감성사전의 부정 단어와 매치된 토큰 개수
#'   \item n_positive: numeric. 감성사전의 긍정 단어와 매치된 토큰 개수
#'   \item n_neutral: numeric. 감성사전의 중립 단어와 매치된 토큰 개수
#'   \item negative: numeric. 감성사전의 부정 단어와 매치된 토큰의 점수의 합
#'   \item positive: character. 감성사전의 긍정 단어와 매치된 토큰의 점수의 합
#'   \item polarity: numeric. 감성의 극성. (positive - negative) / (positive + negative).
#' }
#' @examples
#' \donttest{
#' get_polarity(buzz$CONTENT[1])
#' 
#' # 개별 문서들의 감성분석
#' get_polarity(buzz$CONTENT[1:5])
#' 
#' # 전체 문서를 통합한 감성분석
#' get_polarity(buzz$CONTENT[1:5], indiv = FALSE)
#' }
#' @import dplyr
#' @importFrom stringr str_which str_detect str_extract
#' @importFrom purrr map_df
#' @export
get_polarity <- function(doc, n = 1, indiv = TRUE) {
  data("sentiment_dic")
  
  get_ngram <- function(x, n = 1) {
    morp <- unlist(morpho_mecab(x, type = "morpheme"))
    morp <- morp[stringr::str_which(names(morp), "[^SF]")]
    
    morp <- paste(morp, names(morp), sep = "/")
    
    N <- length(morp)
    term <- character(N - n + 1)
    
    for (i in seq(term)) {
      term[i] <- paste(morp[i:(i+n-1)], collapse = " ")
    }
    
    term
  }
  
  get_polarities <- function(x, n = 1) {
    data.frame(morpheme = get_ngram(x, n = n), stringsAsFactors = FALSE) %>%
      dplyr::mutate(word = stringr::str_remove_all(morpheme, "[\\/\\+[A-Z]]")) %>% 
      dplyr::left_join(sentiment_dic,
                       by = "word") %>%
      dplyr::filter(!is.na(polarity)) %>%
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
    purrr::map_df(get_polarities, n = n) 
  
  if (!indiv) {
    result <- result %>% 
      summarise(
        n_match = sum(n_match),
        n_negative = sum(n_negative),
        n_positive = sum(n_positive),
        n_neutral  = sum(n_neutral),
        negative   = sum(negative),
        positive   = sum(positive),
        polarity   = (positive - negative) / (positive + negative)
      )
  } 

  result
}


