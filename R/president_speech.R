#' President's Speech
#'
#' @description
#' 대통령기록연구실 홈페이지에서 수집한 역대 퇴임 대통령들의 연설문
#'
#' @format 7개의 변수와 2,408개의 관측치로 구성된 티블(tibble) 객체.:
#' \describe{
#'   \item{\code{id}}{character 연설문 아이디}
#'   \item{\code{president}}{character 연설 대통령}   
#'   \item{\code{category}}{character 연설문 분야}
#'   \item{\code{type}}{character 연설문 유형}
#'   \item{\code{title}}{character 연설문 제목}
#'   \item{\code{speech_date}}{double 연설 일자}
#'   \item{\code{doc}}{character 연설문 내용}
#'}
#' @details 역대 대통령 중 김대중, 노무현, 이명박 3명의 대통령 연설문만 수록되어 있음
#' @docType data
#' @keywords datasets
#' @name president_speech
#' @usage data(president_speech)
#' @source
#' "행정안전무 대통령기록관 홈페이지의 기록컬렉션>연설기록 페이지 <http://www.pa.go.kr/research/contents/speech/index.jsp>
#' @examples
#' \dontrun{
#' data(president_speech)
#'
#' head(president_speech)
#' }
NULL
