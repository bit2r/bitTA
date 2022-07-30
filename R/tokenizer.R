#' Extract Collapsed Noun
#' @description 텍스트 문서에서 명사들을 토큰화한 후, 토큰화된 명사들을 공백으로
#' 묶어서 텍스트 문서를 만듦
#' 조회한다.
#' @param doc character. 명사로만 구성될 문서를 만들 대상 텍스트 데이터
#' @param chunk integer. 병렬 작업 수행 시 처리 단위인 chunk
#' @param mc.cores integer. 병렬 작업 수행 시 사용할 코어의 개수
#' @return character. 명사로만 구성된 텍스트
#' @examples
#' \donttest{
#' collapse_noun(president_speech$doc[1:7])
#' 
#' # Collaboration with tidytext
#' library(dplyr)
#' 
#' nho_noun <- president_speech %>%
#'   filter(president %in% "노무현") %>%
#'   filter(str_detect(category, "^외교")) %>%
#'   mutate(doc_noun = collapse_noun(doc)) %>%
#'     tidytext::unnest_ngrams(
#'       noun_bigram,
#'       doc_noun,
#'       n = 2
#'    )
#' nho_noun
#'  
#' nho_noun$noun_bigram[1:5]
#' }
#' @export
#' @import dplyr
#' @import parallel
#' @importFrom purrr map_chr
#' @importFrom tibble is_tibble
collapse_noun <- function(doc,
    chunk = round(length(if (tibble::is_tibble(doc)) dplyr::pull(doc) else doc) / mc.cores),
    mc.cores = parallel::detectCores()) {

  if (tibble::is_tibble(doc)) {
    doc <- pull(doc)
  }
  
  chunk_idx <- get_chunk_id(N = length(doc), chunk = chunk)
    
  get_collapse_noun <- function(chunk_id, doc) {
    start <- chunk_idx$idx_start[chunk_id]
    end <- chunk_idx$idx_end[chunk_id]
    
    tmp <- doc[start:end]
    
    tmp %>% 
      purrr::map_chr(
        function(x) {
          morpho_mecab(x) %>% 
            paste(collapse = " ")
        }
      )
  }
  
  collapsed <- parallel::mclapply(
    seq(chunk_idx$idx_start), 
    get_collapse_noun, 
    doc = doc, 
    mc.cores = mc.cores
  )

  do.call("c", lapply(collapsed, function(x) x))
}

# nho_noun_indiv <- president_speech %>% 
#   filter(president %in% "노무현") %>% 
#   filter(str_detect(category, "^외교")) %>% 
#   tidytext::unnest_tokens(
#     out = "speech_noun",
#     input = "doc",
#     token = morpho_mecab
#   )