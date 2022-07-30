#' part-of-speech tagger based on mecab-ko morphology analyzer
#' @description Mecab 형태소 분석기 기반 형태소분석/품사 태깅을 통한 토큰화
#' @param x character. 형태소 분석에 사용할 document.
#' @param type character. 형태소 분석의 결과 유형.모든 품사, 명사, 동사 및 형용사와 같은
#'  토큰화 결과 유형을 지정.
#'  "morpheme", "noun", "noun2", "verb", "adj"중에서 선택. 기본값은 "noun"로
#'  일반명사만 추출함.
#' @param indiv logical. 복수개의 문서일 때 개별 문서를 리스트로 반환할 지를 선택함.
#' TRUE이면 개별 리스트로 반환하고, FALSE이면 하나의 문자 벡터로 반환함.
#' 기본값은 TRUE
#' @param user_dic mecab-ko 형태소 분석기의 사용자 정의 사전 파일.
#' 기본값은 NULL로 사용자 사전파일을 지정하지 않음.
#' 시스템 사전인 "/usr/local/lib/mecab/dic/mecab-ko-dic"(Linux, Mac)를 보완하여 사용됨.
#' 사용자 사전 파일은 mecab-dict-index 명령어로 생성되며, 확장자가 "dic"임.
#' @details
#' type 인수에 따라 토큰화되는 품사의 종류는 다음과 같다.:
#' \itemize{
#' \item "morpheme" : 모든 품사 토큰화
#' \item "moun" : 일반명사(NNG) 토큰화
#' \item "moun" : 모든 명사 토큰화
#' \item "verb" : 동사 토큰화
#' \item "adj" : 형용사 토큰화
#' }
#'
#' Mecab 형태소 분석기의 시스템 사전의 경로는 "/usr/local/lib/mecab/dic/mecab-ko-dic"이며,
#' NIADic이 포팅되어 들어 있음. 그러나, "/usr/local/lib/mecab/dic/mecab-ko-dic2"에는
#' NIADic이 포함되어 있지 않음. 이것은 bitTA 패키지에서는 참조하지 않음.
#' @return Mecab 형태소 분석기 결과 구조의 character 벡터 혹은 character 벡터를
#' 원소로 갖는 list 객체.
#' @examples
#' \donttest{
#' ## Mecab 형태소 분석
#' morpho_mecab("아버지가 방에 들어가신다.")
#' morpho_mecab("아버지가 방에 들어가신다.", type = "morpheme")
#' morpho_mecab("아버지가 방에 들어가신다.", type = "verb")
#'
#' dic_path <- system.file("dic", package = "bitTA")
#' dic_file <- glue::glue("{dic_path}/buzz_dic.dic")
#'
#' str <- "신혼부부나 주말부부는 놀이공원 자유이용권을 즐겨 구매합니다."
#' morpho_mecab(str)
#' morpho_mecab(str, user_dic = dic_file)
#'
#' morpho_mecab(c("무궁화꽃이 피었습니다.", "나는 어제 올갱이국밥을 먹었다."))
#' morpho_mecab(c("무궁화꽃이 피었습니다.", "나는 어제 올갱이국밥을 먹었다."), indiv = FALSE)
#' 
#' # Using morpho_mecab with tidytext package
#' library(dplyr)
#' 
#' nho_noun_indiv <- president_speech %>%
#'   filter(president %in% "노무현") %>%
#'   filter(str_detect(category, "^외교")) %>%
#'   tidytext::unnest_tokens(
#'     out = "speech_noun",
#'     input = "doc",
#'     token = morpho_mecab
#'   )
#'   
#'  nho_noun_indiv 
#' }
#' @export
#' @import dplyr
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' 
morpho_mecab <- function(x, type = c("noun", "noun2", "verb", "adj", "morpheme"),
                         indiv = TRUE, user_dic = NULL) {
  if (!is_mecab_installed()) {
    stop("To use morpho_mecab(), you need to install mecab-ko and mecab-ko-dic.\nYou can install it with install_mecab_ko().")
  }
  
  packages <- installed.packages()[,1] 
  
  if (!is.element("RcppMeCab", packages)) {
    stop("To use morpho_mecab(), you need to install RcppMeCab package.\nYou can install it with install.packages(\"RcppMeCab\").")
  }  
  
  type <- match.arg(type)
  
  if (is_windows()) {
    x <- iconv(x, "cp949", "utf-8")
  }
  
  if (is.null(user_dic)) {
    morpheme <- RcppMeCab::posParallel(x)
  } else {
    morpheme <- RcppMeCab::posParallel(x, user_dic = user_dic)
  }
  
  tokens <- morpheme %>% 
    purrr::map(
      function(doc) {
        doc %>% 
          stringr::str_split("/") %>% 
          purrr::map(
            function(x) {
              result <- x[1]
              names(result) <- x[2]
              
              result
            }
          ) %>% 
          unlist()
      }
    )
  
  names(tokens) <- NULL
  
  if (type != "morpheme") {
    if (type %in% "noun") pattern <- "NNG"
    if (type %in% "noun2") pattern <- "^N"
    if (type == "verb") pattern <- "^VV"
    if (type == "adj") pattern <- "^VA"
    
    tokens <- tokens %>%
      purrr::map(
        function(token) {
          idx <- stringr::str_detect(names(token), pattern)
          token[idx]
        }
      )
  }
  
  if (!indiv) {
    tokens <- unlist(tokens)
  }
  
  if (length(tokens) == 1) {
    tokens <- unlist(tokens)
  }
  
  tokens
}

#' Korean automatic spacing
#' @description 한글 문장을 띄어쓰기 규칙에 맞게 자동으로 띄어쓰기 보정.
#' @param x character. 띄어쓰기 보정에 사용할 document.
#' @param user_dic mecab-ko 형태소 분석기의 사용자 정의 사전 파일.
#' 기본값은 NULL로 사용자 사전파일을 지정하지 않음.
#' @return 띄어쓰기 보정된 character 벡터.
#' @examples
#' \donttest{
#' # 한글 자동 띄어쓰기
#' get_spacing("최근음성인식정확도가높아짐에따라많은음성데이터가Text로변환되고분석되기시작했는데,이를위해잘동작하는띄어쓰기엔진은거의필수적인게되어버렸다")
#'
#' str <- "글쓰기에서맞춤법과띄어쓰기를올바르게하는것은좋은글이될수있는요건중하나이다.하지만요즘학생들은부족한어문규정지식으로인해맞춤법과띄어쓰기에서많은오류를범하기도한다.본연구는그중띄어쓰기가글을인식하는데중요한역할을하는것으로판단하여,대학생들이띄어쓰기에대해서어느정도정확하게인식하고있는지,실제오류실태는어떠한지에대해살펴서그오류를개선할수있는교육방안을마련할필요가있다고판단하였다."
#' get_spacing(str)
#' }
#' @export
get_spacing <- function(x, user_dic = NULL) {
  mor <- morpho_mecab(x, type = "morpheme", user_dic = user_dic)
  mor <- sapply(mor, c)
  
  ## 조사/어미/접미사/마침표,물음표,느낌표,컴마
  idx <- grep("^J|^E|^XS|SF|SE|NNBC|SC|VCP", names(mor))
  
  for (i in rev(idx)) {
    mor[i-1] <- paste(mor[i-1], mor[i], sep = "")
  }
  
  mor <- mor[-idx]
  
  paste(mor, collapse = " ")
}

