FROM haskell:latest

RUN cabal update
RUN cabal install MissingH
RUN cabal install split
RUN cabal install vector
RUN mkdir -p /haskell
WORKDIR /haskell