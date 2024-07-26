# Stage 1: Build stage
FROM debian:bullseye AS builder

WORKDIR /app

RUN \
  apt-get update -y && \
  apt-get install -y build-essential \
  curl \
  libnuma-dev \
  zlib1g-dev \
  libgmp-dev \
  libgmp10 \
  git \
  wget \
  lsb-release \
  software-properties-common \
  gnupg2 \
  apt-transport-https \
  gcc \
  autoconf \
  automake \
  pkg-config

RUN \
  curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup -o /usr/bin/ghcup && \
  chmod +x /usr/bin/ghcup

ARG GHC=9.4.8
ARG CABAL=3.6

RUN \
  ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
  ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL}

ENV PATH="/root/.ghcup/bin:${PATH}"

COPY . .

RUN \
  cabal update && \
  cabal install --installdir=/app exe:biocad && \
  cabal build

# Stage 2: Runtime stage
FROM debian:bullseye-slim

WORKDIR /app

COPY --from=builder /app/biocad /app/biocad

RUN chmod +x /app/biocad

RUN apt-get update && \
  apt-get install -y libnuma1 libgmp10 && \
  apt-get autoremove -y && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

EXPOSE 8080

CMD ["/app/biocad"]