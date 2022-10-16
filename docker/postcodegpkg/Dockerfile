FROM ubuntu:22.04

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && \
  apt-get --no-install-recommends -y install gnupg software-properties-common dirmngr wget locales  \
    wget curl libssl-dev libssh2-1-dev libssl3 libcurl4-gnutls-dev \
    libxml2-dev \
    gdal-bin libgdal-dev proj-data proj-bin libgeos-dev libudunits2-dev && \
  sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
  dpkg-reconfigure --frontend=noninteractive locales && \
  update-locale LANG=en_US.UTF-8 && \
  wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc && \
  add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" && \
  apt-get update && \
  apt-get --no-install-recommends -y install r-base r-base-core r-base-dev r-recommended && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* && \
  echo 'LC_ALL=en_US.UTF-8' >> /etc/R/Renviron.site

RUN echo "MAKE = make -j20" >> /etc/R/Makeconf && \
  R -e "install.packages(c('RSQLite', 'sf', 'dplyr', 'httr', 'rvest'), repos='https://cran.rstudio.com')"

COPY ./nz_postcode_wpc.R /

CMD Rscript nz_postcode_wpc.R