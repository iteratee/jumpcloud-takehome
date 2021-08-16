# syntax=docker/dockerfile:1
FROM haskell:8.10.4
WORKDIR /yummly-takehome
COPY . .
RUN apt update && apt install libexpat1-dev
RUN cabal update && cabal v2-build jumpcloud-takehome-test-hunit-plus
CMD ["cabal", "v2-test"]
