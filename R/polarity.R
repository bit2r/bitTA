#' KOSAC(Korean Sentiment Analysis Corpus) sentiment dictionary
#'
#' @description
#' 서울대학교 언어학과에서 세종 구문분석 코퍼스로부터 선별한 332개 신문기사의
#' 7,744 문장을 주석 대상으로 구축한 한국어 감정 코퍼스
#'
#' @format 10개의 변수와 15,736개의 관측치로 구성된 데이터 프레임.:
#' \describe{
#'   \item{ngram}{character. N-GRAM}
#'   \item{freq}{integer. 빈도수}
#'   \item{COMP}{numeric. complex 확률}
#'   \item{NEG}{numeric. negative 확률}
#'   \item{NEUT}{numeric. neutral 확률}
#'   \item{None}{numeric. none 확률}
#'   \item{POS}{numeric. positive 확률}
#'   \item{max.value}{character. max 항목}
#'   \item{max.prop}{numeric. max 항목 확률}
#'   \item{type}{factor. 구분. "kosac"}
#' }
#' @docType data
#' @keywords datasets
#' @name polarity
#' @usage data(polarity)
#' @source 
#' Korean Sentiment Analysis Corpus homepage. http://word.snu.ac.kr/kosac/index.php
#' @examples
#' \dontrun{
#' data(polarity)
#'
#' head(polarity)
#' }
NULL
