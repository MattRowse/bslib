FROM rocker/r-ver:3.5.0

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    file \
    libcurl4-openssl-dev \
    libedit2 \
    libssl-dev \
    lsb-release \
    psmisc \
    procps \
    wget \
    libxml2-dev \
    libpq-dev \
    libssh2-1-dev \
    ca-certificates \
    libglib2.0-0 \
	libxext6 \
	libsm6  \
	libxrender1 \
	bzip2 \
    apache2 \
    zlib1g-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ 
    
RUN R -e "install.packages('SnowballC')"
RUN R -e "install.packages('tm')"
RUN R -e "install.packages('tidytext')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('rjson')"
RUN R -e "install.packages('e1071')"
RUN R -e "install.packages('gsheet')"
RUN R -e "install.packages('urltools')"
RUN R -e "install.packages('openssl')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('Rtools')"
RUN R -e "install.packages('sass')"
RUN R -e "install.packages('bslib',dependencies=TRUE)"
RUN R -e "install.packages('thematic',dependencies=TRUE)"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('lubridate')"

COPY / /

EXPOSE 80 443

WORKDIR /Chat_App

CMD "R" "-e" "shiny::runApp('app.R', host='0.0.0.0')"