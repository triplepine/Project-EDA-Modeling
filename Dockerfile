# start from the rstudio/plumber image
FROM rocker/r-ver:4.4.1

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev  libpng-dev
    
# Install R packages needed
RUN R -e "install.packages(c('GGally', 'plumber', 'readr', 'dplyr', 'caret', 'rpart', 'Metrics'))"

# Set the working directory
WORKDIR /app

# copy everything from the current directory into the container
COPY modelapi.R modelapi.R
COPY processed_diabetes.rds processed_diabetes.rds
COPY cl_tree_fit.rds cl_tree_fit.rds

# open port to traffic
EXPOSE 8000

# when the container starts, start the myAPI.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('modelapi.R'); pr$run(host='0.0.0.0', port=8000)"]