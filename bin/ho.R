suppressPackageStartupMessages(library(tidyverse))
library(stringi)

args <- commandArgs(trailingOnly = TRUE)

if (args[1] == "ALL") {
  files <- dir("ho-src")
  files <- files[stri_sub(files, 1, 2) == "ho"]
  files <- as.numeric(stri_sub(files, 4, 5))
  args <- files
}

args <- as.numeric(args)
for (ar in args)
{
  cat(sprintf("HANDOUT %02d\n", ar))

  # filenames
  f1 <- sprintf("ho-%02d.tex", ar)
  f2 <- sprintf("ho-%02d.pdf", ar)

  # compile the version to print
  system(sprintf("cd ho-src; Rpdflatex %s", f1))

  # move to output
  file.rename(file.path("ho-src", f2), file.path("ho", f2))
}