FROM centos:7

MAINTAINER Luiz Gadelha "lgadelha@lncc.br"

# =============
# --- Linux ---
# =============
RUN yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm && \
    yum -y update && \
    yum -y install libpng-devel proj-devel gdal-devel  proj-epsg proj-nad v8-devel libcurl-devel geos-devel libxml2-devel openssl-devel R wget && \
    yum -y clean all

# =============
# --- Shiny ---
# =============
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/centos-6.3/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/centos-6.3/x86_64/shiny-server-$VERSION-rh6-x86_64.rpm" -O ss-latest.rpm && \
    yum -y install ss-latest.rpm && \
    mkdir /usr/share/doc/R-3.3.1/html/ && \
    rm -f version.txt ss-latest.rpm && \
    yum -y clean all

RUN R -e "install.packages('XML', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('maps', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('rgdal', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('raster', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('dismo', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('rjson', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('leaflet', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('shiny', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('shinythemes', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('html_vignette', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('digest', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('rgbif', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('shinydashboard', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('randomForest', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('kernlab', repos='https://cran.fiocruz.br/', clean=TRUE)"
RUN R -e "install.packages('rJava', repos='https://cran.fiocruz.br/', clean=TRUE)"

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod 755 /usr/bin/shiny-server.sh
COPY Model-R /srv/shiny-server/Model-R
RUN chmod -R 777 /srv/shiny-server/Model-R
CMD /usr/bin/shiny-server.sh
