FROM rocker/shiny:3.5.1

MAINTAINER Paul Stevenson "pstevenson6@gmail.com"

RUN apt-get update && apt-get install libcurl4-gnutls-dev libv8-3.14-dev libgit2-dev libssl-dev -y &&\
    mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Download and install library
RUN R -e "install.packages(c('dplyr', 'tidyr', 'ggplot2', 'shinydashboard', 'shinyjs', 'V8', 'DT', 'lubridate', 'httr', 'jsonlite', 'googleAuthR', 'devtools'))" && \
    R -e "devtools::install_github('MarkEdmondson1234/googleID')"

# copy the app to the image
COPY shinyapps /root/app
COPY Rprofile.site /usr/local/lib/R/etc/Rprofile.site

# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
RUN chmod -R 755 /root/app
RUN chmod -R 755 /usr/local/lib/R/etc

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app')"]
