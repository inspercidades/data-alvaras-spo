FROM rocker/geospatial:4.5.1

# Pinned to a rocker/geospatial tag so GDAL, PROJ, and GEOS stay fixed.
# Update the tag deliberately, then re-render the pipeline and re-check
# the validation report.

RUN R -e "install.packages('renv')"

WORKDIR /home/rstudio/project
COPY renv.lock renv.lock
RUN R -e "renv::restore()"

COPY . .

CMD ["R", "-e", "targets::tar_make()"]
