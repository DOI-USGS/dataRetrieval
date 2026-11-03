FROM code.chs.usgs.gov:5001/ctek/docker/r-lang/r-base:4.6

# System dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
  python3 \
  python3-pip \
  python3-dev \
  # These might already come with r-cran-sf binary, commenting out for now:
  # libgdal-dev \
  # libgeos-dev \
  # libproj-dev \
  && rm -rf /var/lib/apt/lists/*

# Python dependencies
RUN pip3 install --no-cache-dir --break-system-packages \
  dataretrieval \
  matplotlib \
  geopandas \
  numpy \
  pandas \
  scipy \
  folium>=0.12 \
  mapclassify \
  python-dateutil \
  seaborn \
  requests

# R dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  r-cran-reticulate \
  r-cran-curl \
  r-cran-lubridate \
  r-cran-xml2 \
  r-cran-httr2 \
  r-cran-dplyr \
  r-cran-sf \
  r-cran-ggplot2 \
  r-cran-tidyr \
  r-cran-purrr \
  r-cran-jsonlite \
  r-cran-knitr \
  r-cran-rmarkdown \
  r-cran-leaflet \
  r-cran-data.table \
  r-cran-readr \
  r-cran-rcmdcheck \
  r-cran-testthat \
  r-cran-pkgdown \
  r-cran-covr \
  r-cran-readxl \
  r-cran-dbi \
  r-cran-duckdb \
  r-cran-dbplyr \
  r-cran-dt \
  && rm -rf /var/lib/apt/lists/*

ENV RETICULATE_PYTHON=/usr/bin/python3

COPY *.tar.gz /tmp/pkg.tar.gz
RUN set -eux; \
  R CMD INSTALL /tmp/pkg.tar.gz && \
  Rscript -e "library(dataRetrieval)" && \
  rm -f /tmp/*.tar.gz
