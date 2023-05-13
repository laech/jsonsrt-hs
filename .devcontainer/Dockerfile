FROM mcr.microsoft.com/devcontainers/base:bullseye

RUN apt-get update \
  && apt-get install -y libgmp-dev libtinfo-dev \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/*
