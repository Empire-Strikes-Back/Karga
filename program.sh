#!/bin/bash

repl(){
  clj \
    -J-Dclojure.core.async.pool-size=1 \
    -X:repl Ripley.core/process \
    :main-ns Karga.main
}


main(){
  clojure \
    -J-Dclojure.core.async.pool-size=1 \
    -M -m Karga.main
}

jar(){

  clojure \
    -X:identicon Zazu.core/process \
    :word '"Karga"' \
    :filename '"out/identicon/icon.png"' \
    :size 256

  rm -rf out/*.jar
  clojure \
    -X:uberjar Genie.core/process \
    :main-ns Karga.main \
    :filename "\"out/Karga-$(git rev-parse --short HEAD).jar\"" \
    :paths '["src"]'
}

release(){
  jar
}

"$@"