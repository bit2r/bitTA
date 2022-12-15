get_meta_person <- function(x, userdic_path = NULL) {
  if (is_windows()) {
    installd <- "c:/mecab"
    dic_path <- "mecab-ko-dic/user-dic"    
    
    userdic_path <- glue::glue("{installd}/{dic_path}")    
  } else {
    installd <- '/usr/local/install_resources' 
    dic_path <- "mecab-ko-dic-2.1.1-20180720/user-dic"   
    
    userdic_path <- glue::glue("{installd}/{dic_path}")
  }

  meta <- read_csv(glue::glue("{userdic_path}/person.csv"), col_names = FALSE)
  meta
}

add_userdic_person <- function(x, userdic_path = NULL) {
  if (is_windows()) {
    installd <- "c:/mecab"
    dic_path <- "mecab-ko-dic/user-dic"    
    
    userdic_path <- glue::glue("{installd}/{dic_path}")    
  } else {
    installd <- '/usr/local/install_resources' 
    dic_path <- "mecab-ko-dic-2.1.1-20180720/user-dic"   
    
    userdic_path <- glue::glue("{installd}/{dic_path}")
  }
  
  meta <- read_csv(glue::glue("{userdic_path}/person.csv"), col_names = FALSE)
  meta
}
