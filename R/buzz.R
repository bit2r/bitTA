#' Naver Cafe Post Scraping Data
#' 
#' @description 
#' 네이버 카페의 게시판에 올라온 포스트를 맞벌이, 워킹맘 등의 몇몇 키워드로 스크랩핑한 데이터
#' 
#' @format 13개의 변수와 1,000개의 관측치로 구성된 데이터 프레임.:
#' \describe{
#'   \item{KEYWORD}{character. 컨텐츠 키워드}
#'   \item{SRC}{character. 컨텐츠 등록 메뉴}
#'   \item{SECTION}{character. 컨텐츠 섹션}
#'   \item{CRAWL_DT}{character. 크롤링 일시}
#'   \item{PUBLISH_DT}{character. 컨텐츠 등록 일자}
#'   \item{URL}{character. 컨텐츠 URL}
#'   \item{TITLE}{character. 컨텐츠 제목}
#'   \item{CONTENT}{character. 컨텐츠 내용}
#'   \item{DOC_KEY}{character. 컨텐츠 키}
#'   \item{PUBLISH_ID}{character. 컨텐츠 등록자 아이디}
#'   \item{CLICK_CNT}{integer. 클릭 건수}
#'   \item{LIKE_CNT}{integer. 좋아요 건수}
#'   \item{SEARCH_KEYWORD}{character. 검색 키워드 영문}
#' }
#' @docType data
#' @keywords datasets
#' @name buzz
#' @usage data(buzz)
#' @examples
#' \dontrun{
#' data(buzz)
#' 
#' head(buzz)
#' }
NULL
