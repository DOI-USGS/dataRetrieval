FROM code.chs.usgs.gov:5001/ctek/docker/r-lang/r-base:4.5

# Disable the annoying bell on WSL2
RUN sed -i 's/^# set bell-style none$/set bell-style none/' /etc/inputrc
RUN echo 'set visualbell' >> /root/.vimrc

# Change the name of this environment to something which pleases you, if you
# so please. But the name will not be relevant for most cases, as reticulate
# will be pointed to the environment no matter what it is named.
ARG CONDA_ENVIRONMENT_NAME=dataretrieval

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  wget \
  && rm -rf /var/lib/apt/lists/*
  
RUN wget -O Miniforge3.sh "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh" &&  \
    bash Miniforge3.sh -b -p "${HOME}/conda" && \
    rm Miniforge3.sh
ENV CONDA_DIR="/root/conda"
ENV PATH=$CONDA_DIR/bin:$PATH
COPY environment.yml /
RUN conda env create -n ${CONDA_ENVIRONMENT_NAME} -f /environment.yml && conda clean -a -y

# Necessary R libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
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

ENV RETICULATE_PYTHON=/root/conda/envs/${CONDA_ENVIRONMENT_NAME}/bin/python
