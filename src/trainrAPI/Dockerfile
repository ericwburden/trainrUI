FROM rstudio/plumber
MAINTAINER Eric Burden "eric.w.burden@gmail.com"

RUN apt-get update && apt-get install -y \
    sudo

RUN R -e "install.packages('glue', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('stringr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('testthat', repos='https://cloud.r-project.org/')"

RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"
RUN R -e "remotes::install_github('ericwburden/trainr@0.7.1')"

# Install API
COPY ./plumber.R /app/plumber.R
RUN useradd -ms /bin/bash plumber
RUN chown -R plumber:plumber /app

EXPOSE 3838

USER plumber

CMD ["/app/plumber.R"]
