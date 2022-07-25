
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bitTA <img src="man/figures/r2bit.png" align="right" height="120" width="130"/>

## Overview

`bitTA`는 텍스트 데이터의 품질을 보정하고,
`자연어 처리(Natural Language Processing)` 및 `형태소분석`, `감성분석`을
지원하는 도구의 모음입니다.

`bitTA`의 다음 기능은 bitTA 패키지의 비네트인 [Introduce
bitTA](https://r2bit.com/bitTA/articles/Introduce.html)에 소개되어
있습니다.

-   텍스트 데이터 전처리 기능
-   텍스트 데이터 품질 진단 기능
-   형태소분석 기능
-   감성분석 기능

## Install bitTA

### bitTA 패키지 설치하기

Github 리파지토리에서 배포하는 패키지를 다음과 같이 설치합니다.

``` r
remotes::install_github("bit2r/bitTA")
```

### 관련 리소스 설치하기

bitTA를 사용하기 위해서는 다음의 두 리소스를 설치해야 합니다.

-   은전한닢 형태소분석기 시스템과 사전
    -   mecab-ko 혹은 mecab-ko-msvc
    -   mecab-ko-dic
-   RcppMeCab 패키지
    -   R에서 mecab-ko 연동을 위한 R 패키지

은전한닢 형태소분석기 시스템과 사전은 bitTA 패키지의 비네트인 [Install
mecab-ko](https://r2bit.com/bitTA/articles/Install_mecab.html)에
설명되어 있습니다.

사전에 설치해야 하는 리소스는 다음의 순서와 방법대로 설치하는 것을
추천합니다.

1.  은전한닢 형태소분석기 시스템과 사전

``` r
library("bitTA")

install_mecab_ko()
```

2.  RcppMeCab 패키지 설치

``` r
install.packages("RcppMeCab")
```

## bitTA 사용하기

### 한글 자동 띄어쓰기

한글 문장을 띄어쓰기 규칙에 맞게 자동으로 띄어쓰기 보정

``` r
library(bitTA)

get_spacing("최근음성인식정확도가높아짐에따라많은음성데이터가Text로변환되고분석되기시작했는데,이를위해잘동작하는띄어쓰기엔진은거의필수적인게되어버렸다")
#> [1] "최근 음성 인식 정확도가 높아 짐에 따라 많은 음성 데이터가 T ex t로 변환되고 분석되기 시작했는데, 이를 위해 잘 동작하는 띄어쓰기 엔진은 거의 필수적인 게 되어 버렸다"
str <- "글쓰기에서맞춤법과띄어쓰기를올바르게하는것은좋은글이될수있는요건중하나이다.하지만요즘학생들은부족한어문규정지식으로인해맞춤법과띄어쓰기에서많은오류를범하기도한다.본연구는그중띄어쓰기가글을인식하는데중요한역할을하는것으로판단하여,대학생들이띄어쓰기에대해서어느정도정확하게인식하고있는지,실제오류실태는어떠한지에대해살펴서그오류를개선할수있는교육방안을마련할필요가있다고판단하였다."
get_spacing(str)
#> [1] "글쓰기에서 맞춤법과 띄어쓰기를 올바르게 하는 것은 좋은 글이 될 수 있는 요건 중 하나이다. 하지만 요즘 학생들은 부족한 어문 규정 지식으로 인해 맞춤법과 띄어쓰기에서 많은 오류를 범하기도 한다. 본 연구는 그중 띄어쓰기가 글을 인식하는 데 중요한 역할을 하는 것으로 판단하여, 대학생들이 띄어쓰기에 대해서 어느 정도 정확하게 인식하고 있는지, 실제 오류 실태는 어떠한지에 대해 살펴서 그 오류를 개선할 수 있는 교육 방안을 마련할 필요가 있다고 판단하였다."
```

### 형태소 분석

은전한닢 형태소 분석기를 호출하여 형태소 분석을 수행

형태소분석은 비네트인 [Morphological
Analysis](https://r2bit.com/bitTA/articles/morphology.html)에 설명되어
있습니다.

``` r
morpho_mecab("아버지가 방에 들어가신다.")
#>      NNG      NNG 
#> "아버지"     "방"

morpho_mecab("아버지가 방에 들어가신다.", type = "morpheme")
#>      NNG      JKS      NNG      JKB       VV    EP+EF       SF 
#> "아버지"     "가"     "방"     "에" "들어가"   "신다"      "."

morpho_mecab("아버지가 방에 들어가신다.", type = "verb")
#>       VV 
#> "들어가"
```

### 감성 분석

#### Mecab 형태소 분석기

Mecab 형태소 분석기를 호출하여 형태소 분석을 수행

``` r
morpho_mecab("아버지가 방에 들어가신다.")
#>      NNG      NNG 
#> "아버지"     "방"

morpho_mecab("아버지가 방에 들어가신다.", type = "morpheme")
#>      NNG      JKS      NNG      JKB       VV    EP+EF       SF 
#> "아버지"     "가"     "방"     "에" "들어가"   "신다"      "."

morpho_mecab("아버지가 방에 들어가신다.", type = "verb")
#>       VV 
#> "들어가"
```
