FROM code.chs.usgs.gov:5001/ctek/docker/r-lang/r-base:4.4

# Necessary R libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  r-cran-oce \
  r-cran-devtools \
  r-cran-here \
  r-cran-rmarkdown \
  r-cran-knitr \
  r-cran-dt \
  r-cran-data.table \
  r-cran-gridextra \
  r-cran-tidyverse \
  r-cran-jsonlite \
  r-cran-readr \
  r-cran-xml2 \
  r-cran-httr2 \
  r-cran-rsconnect \
  r-cran-connectapi \
  r-cran-covr \
  r-cran-sf \
  r-cran-zoo \
  r-cran-patchwork \
  r-cran-maps \
  r-cran-leaflet \
  r-cran-readxl \
  && apt-get install -y pandoc \
  && rm -rf /var/lib/apt/lists/*


