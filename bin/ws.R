suppressPackageStartupMessages(library(tidyverse))
library(stringi)

args <- commandArgs(trailingOnly = TRUE)

if (args[1] == "ALL") {
  files <- dir("ho-src")
  files <- files[stri_sub(files, 1, 2) == "ho"]
  files <- as.numeric(stri_sub(files, 4, 5))
  args <- files
}

trim_chr <- function(v)
{
  index <- cumsum(v != "")
  v <- v[index > 0]
  index <- rev(cumsum(rev(v) != ""))
  v <- v[index > 0]
  v
}

header <- read_lines("ws-src/header.tex")

args <- as.numeric(args)
for (ar in args)
{
  cat(sprintf("WORKSHEET %02d\n", ar))

  # filenames
  f1 <- sprintf("ws-%02d.tex", ar)
  f2 <- sprintf("sl-%02d.tex", ar)

  # find the keys
  page <- read_lines(file.path("ws-src", f1))
  page <- page[stri_sub(page, 1L, 2L) != "%%"]
  tps <- c("% QUESTION", "% SOLUTION")
  qid <- cumsum(stri_sub(page, 1L, 10L) %in% tps)
  page <- page[qid > 0]; qid <- qid[qid > 0]

  elem <- split(page, qid)
  h1 <- stri_trim(map_chr(elem, getElement, 1L))
  if (!all(h1 == tps)) { print(h1) ; stop("Misaligned Qs and As") }
  elem <- map(elem, ~ ..1[seq(2L, length(..1))] )
  elem <- map(elem, trim_chr)
  qs <- elem[seq(1L, length(elem) - 1L, by = 2L)]
  as <- elem[seq(2L, length(elem), by = 2L)]
  nums <- seq_along(qs)
  stopifnot(length(qs) == length(as))

  # make the worksheet
  qs1 <- map2(qs, nums, function(u, v) {
    u[1] <- sprintf("\\textbf{%d}. %s", v, u[1])
    u <- c(u, "")
    u
  })
  out <- c(header, sprintf("{\\LARGE Worksheet %02d}", ar),
           "", "\\vspace*{18pt}", "", "", unlist(qs1),
           "", "\\end{document}")
  write_lines(out, file.path("ws", f1))
  system(sprintf("cd ws; Rxelatex %s", f1))

  # make the solutions
  as1 <- map(as, function(u) {
    u[1] <- sprintf("\\textit{Solution:} %s", u[1])
    u <- c(u, "")
    u
  })
  both <- map2(qs1, as1, c)
  out <- c(header, sprintf("{\\LARGE Worksheet %02d (Solutions)}", ar),
           "", "\\vspace*{18pt}", "", "", unlist(both),
           "", "\\end{document}")
  out <- out[!stri_detect(out, fixed = "%NOSOLUTIONS")]
  write_lines(out, file.path("stage/sl-stage", f2))
  system(sprintf("cd stage/sl-stage; Rxelatex %s", f2))  
}