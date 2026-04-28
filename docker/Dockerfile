FROM code.chs.usgs.gov:5001/ctek/docker/r-lang/r-base:4.5

# Change the name of this environment to something which pleases you, if you
# so please. But the name will not be relevant for most cases, as reticulate
# will be pointed to the environment no matter what it is named.
ARG CONDA_ENVIRONMENT_NAME=dataretrieval

ENV CONDA_DIR="/root/conda"
ENV PATH=$CONDA_DIR/bin:$PATH
COPY environment.yml /

# Necessary R libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    build-essential \
    wget \
    curl \
    libssl-dev \
    zlib1g-dev \
    libncurses5-dev \
    libncursesw5-dev \
    libreadline-dev \
    libsqlite3-dev \
    libgdbm-dev \
    libdb5.3-dev \
    libbz2-dev \
    libexpat1-dev \
    liblzma-dev \
    tk-dev \
    libffi-dev \
    uuid-dev \
    ca-certificates \
    r-cran-rcmdcheck \
    r-cran-testthat \
    r-cran-pkgdown \
    r-cran-rmarkdown \
    r-cran-knitr \
    r-cran-dt \
    r-cran-data.table \
    r-cran-dplyr \
    r-cran-purrr \
    r-cran-lubridate \
    r-cran-tidyr \
    r-cran-jsonlite \
    r-cran-readr \
    r-cran-xml2 \
    r-cran-httr2 \
    r-cran-covr \
    r-cran-sf \
    r-cran-patchwork \
    r-cran-leaflet \
    r-cran-readxl \
    r-cran-whisker \
    r-cran-ggplot2 \
    r-cran-reticulate \
    && rm -rf /var/lib/apt/lists/* 

# Install specific Python version from source
ENV PYTHON_VERSION=3.11.8

RUN cd /usr/src && \
    wget https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tgz && \
    tar xzf Python-${PYTHON_VERSION}.tgz && \
    cd Python-${PYTHON_VERSION} && \
    ./configure --enable-optimizations && \
    make -j"$(nproc)" && \
    make altinstall && \
    ln -s /usr/local/bin/python3.11 /usr/bin/python3 && \
    ln -s /usr/local/bin/pip3.11 /usr/bin/pip3 && \
    cd / && rm -rf /usr/src/Python-${PYTHON_VERSION}*

# Install Python packages
RUN pip3 install --no-cache-dir \
    dataretrieval 

ENV RETICULATE_PYTHON=/usr/local/bin/python3.11