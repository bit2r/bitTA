#!/bin/bash
set -e

# 설치 리소스를 저장할 디렉토리 
INSTALLD='/usr/local/install_resources' 

#---------------------------------------------
# 은전한닙 형태소분석기 설치
#---------------------------------------------
mkdir -p ${INSTALLD}
cd ${INSTALLD}
wget https://bitbucket.org/eunjeon/mecab-ko/downloads/mecab-0.996-ko-0.9.2.tar.gz 
tar xzvf mecab-0.996-ko-0.9.2.tar.gz 
cd ${INSTALLD}/mecab-0.996-ko-0.9.2 
./configure 
make 
make install
ldconfig 

# 설치파일 삭제
rm -rf $INSTALLD/mecab-0.996-ko-0.9.2 
rm -rf $INSTALLD/mecab-0.996-ko-0.9.2.tar.gz

#---------------------------------------------
# 은전한닙 형태소분석기 사전 설치
#---------------------------------------------
cd ${INSTALLD}
wget https://bitbucket.org/eunjeon/mecab-ko-dic/downloads/mecab-ko-dic-2.1.1-20180720.tar.gz 
tar xvfz mecab-ko-dic-2.1.1-20180720.tar.gz 
cd ${INSTALLD}/mecab-ko-dic-2.1.1-20180720 
autoreconf 
./configure
make
make install

# 설치파일 삭제    
rm -rf $INSTALLD/mecab-ko-dic-2.1.1-20180720.tar.gz
