#' Highway rest area related buzz
#'
#' @description
#' 네이버와 다음 카페와 블로그의 고속도로 휴계소 키워드로 수집한 텍스트 데이터임
#'
#' @format 5개의 변수와 26,168개의 관측치로 구성된 티블(tibble) 객체.:
#' \describe{
#'   \item{SITE_TYPE}{character. 게시물 사이트 유형. "BLOG", "CAFE" 중 하나의 값을 가짐}
#'   \item{SITE}{character. 게시물 사이트. "DAUM", "NAVER"중 하나의 값을 가짐}
#'   \item{PUBLISH_DT}{character. 게시물 등록일자로, "YYMMDD" 포맷의 텍스트 데이터}
#'   \item{TITLE}{character. 게시물 제목}
#'   \item{CONTENT}{character. 게시물 본문}
#' }
#' @docType data
#' @keywords datasets
#' @name rest_area
#' @usage data(rest_area)
#' @examples
#' \dontrun{
#' data(rest_area)
#'
#' head(rest_area)
#' }
NULL

