# hash:sha256:0ddd2bf798c01a42f2d67e8b190e52fa137d41b6485e8d79bb4105b59c36732c
FROM registry.codeocean.com/codeocean/r-base:3.5.3-ubuntu18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        pandoc=1.19.2.4~dfsg-1build4 \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'remotes::install_version("survey", version = "3.36")' \
    && Rscript -e 'remotes::install_version("tidyverse", version = "1.2.1")'
