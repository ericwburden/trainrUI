FROM r-base:4.0.5

MAINTAINER Eric Burden "eric.w.burden@gmail.com"

# Install necessary linux packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libssl-dev \
    libxt-dev \
    xtail \
    wget

# Install Shiny Server
RUN wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.16.958-amd64.deb
RUN gdebi -n shiny-server-1.5.16.958-amd64.deb
RUN rm shiny-server-1.5.16.958-amd64.deb
RUN rm /srv/shiny-server/index.html && rm -rf /srv/shiny-server/sample-apps

# Setup Shiny Server
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN echo "local({options(shiny.port=3838, shiny.host='0.0.0.0')})" > /usr/lib/R/etc/Rprofile.site
RUN chown -R shiny:shiny /var/lib/shiny-server
RUN chown -R shiny:shiny /var/log/shiny-server
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chown shiny:shiny /usr/bin/shiny-server.sh

# Install R Packages
RUN R -e "install.packages('glue', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('httr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('jsonlite', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rredis', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shiny', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyAce', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyjs',, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinythemes', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('stringr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('uuid', repos='https://cloud.r-project.org/')"

RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"
RUN R -e "remotes::install_github('ericwburden/trainr@0.7.1')"

# Install Shiny App
COPY . /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server

RUN echo "CONTAINERIZED=\"TRUE\"" >> /usr/lib/R/etc/Renviron.site

EXPOSE 3838

USER shiny

CMD ["/usr/bin/shiny-server.sh"]
