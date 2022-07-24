#' n-gram Tokenization
#' @description n-gram 토큰화 및 n-gram 토큰화 집계.
#' @param x character. n-gram 토큰화에 사용할 document.
#' @param n integer. n-gram 토큰화에서의 n.
#' @param token character. n-gram 토큰화에서의 n.
#' @param type character. 반환하는 결과물의 종류. "raw"는 토큰화된 n-gram 자체를 반환하며,
#' "table"은 토큰화된 n-gram 집계 정보를 반환.
#' @param user_dic mecab-ko 형태소 분석기의 사용자 정의 사전 파일.
#' 기본값은 NULL로 사용자 사전파일을 지정하지 않음.
#' 
#' @return n-gram 토큰화된 character 벡터, 혹은 n-gram 집계 정보를 담은 데이터 프레임
#' @section n-gram 집계 정보:
#' n-gram 집계 정보를 담은 데이터 프레임 변수는 다음과 같음.:
#' \itemize{
#' \item ngrams : n-gram 토큰. character.
#' \item freq : n-gram 토큰의 도수. integer.
#' \item prop : n-gram 토큰의 상대도수. numeric.
#' }
#' @examples
#' \donttest{
#' str <- "신혼부부나 주말부부는 놀이공원 자유이용권을 즐겨 구매합니다."
#' 
#' # bi-gram
#' get_ngram(str)
#' 
#' # tri-gram
#' get_ngram(str, n = 3)
#' 
#' # 워드(띄어쓰기) 기반 토큰화
#' get_ngram(str, token = "word")
#' 
#' # 집계정보
#' get_ngram(str, type = "table")
#' 
#' # 사용자 정의 함수 사용
#' dic_path <- system.file("dic", package = "bitTA")
#' dic_file <- glue::glue("{dic_path}/buzz_dic.dic")
#' get_ngram(str, user_dic = dic_file)
#' 
#' }
#' 
#' @importFrom ngram ngram get.ngrams ng_order get.phrasetable
#' @export
get_ngram <- function(x, n = 2, token = c("noun", "word"), 
                      type = c("raw", "table"),
                      user_dic = NULL) {
  token <- match.arg(token)
  type <- match.arg(type)
  
  if (token %in% "word") {
    ng <- ngram::ngram(str, n = n, sep = " ")
  } else if (token %in% "noun") {
    ng <- morpho_mecab(x, type = "noun2", user_dic = user_dic) %>% 
      paste(collapse = " ") %>% 
      ngram::ngram(n = n, sep = " ")
  }
  
  if (type %in% "raw") {
    ngram::get.ngrams(ng)[ngram::ng_order(ng)]
  } else if (type %in% "table") {
    ngram::get.phrasetable(ng)
  }
}
