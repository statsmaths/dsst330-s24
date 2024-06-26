suppressPackageStartupMessages({library(tidyverse)
library(lubridate)
library(stringi)
library(xml2)})

args <- commandArgs(trailingOnly = TRUE)

# read the HTML dataset
obj <- read_html(file.path("dev.html"))

# find the links and make open in new tab
links <- xml_find_all(obj, ".//a")
xml_set_attr(links, "target", "_blank")
xml_set_attr(links, "rel", "noreferrer noopener")

# find the links that have a class attribute
clses <- xml_attr(links, "class")
links <- links[!is.na(clses)]
dates <- ymd(clses[!is.na(clses)])
types <- stri_sub(clses[!is.na(clses)], 12L, -1L)

# When should each link become available?
avail <- dates
avail[types == 'ex'] <- dates[types == 'ex']
avail[types == 'ho'] <- dates[types == 'ho']
avail[types == 'ws'] <- dates[types == 'ws']
avail[types == 'nb'] <- dates[types == 'nb'] + 1L
avail[types == 'sl'] <- dates[types == 'sl'] + 1L

# Make sure available solutions are available
td <- as_date(now("America/New_York") + 60 * 60 * 13.25)
these <- which((td >= avail) & (types == 'sl'))
for (k in seq_along(these))
{
  pout <- xml_attr(links[these[k]], "href")
  psrc <- file.path("stage/sl-stage", basename(pout))
  if (file.exists(psrc)) {
    file.copy(psrc, pout, overwrite = TRUE)
  }
}

these <- which((td >= avail) & (types == 'nb'))
for (k in seq_along(these))
{
  pout <- xml_attr(links[these[k]], "href")
  psrc <- file.path("stage/pg-stage", basename(pout))
  if (file.exists(psrc)) {
    file.copy(psrc, pout, overwrite = TRUE)
  }
}

# take the dates after today; modify those links into spans, remove href; if
# after 13h15 (time of the last class) advance an extra day; also, remove
# links that do not exist
if (length(args) == 0 || (args[1] != "notime"))
{
  index <- which(td < avail) 
  if (length(index)) {
    links <- links[index]
    xml_set_name(links, "span")
    xml_set_attr(links, "href", NULL)
    xml_set_attr(links, "class", "futurelink")  
  }
}

# remove missing links
links <- xml_find_all(obj, ".//a")
links <- links[!is.na(xml_attr(links, "class"))]
index <- which(!file.exists(xml_attr(links, "href")))
if (length(index)) {
  links <- links[index]
  xml_set_name(links, "span")
  xml_set_attr(links, "href", NULL)
  xml_set_attr(links, "class", "futurelink")
}

# set last updated time
footer <- xml_find_all(obj, ".//span[@class='footer']")
tnow <- format(now("America/New_York"), "%Y-%m-%dT%H:%M:%S%z")
xml_text(footer) <- sprintf("last updated %s", as.character(tnow))

# write the file
write_html(obj, file.path("index.html"), options = "format")
