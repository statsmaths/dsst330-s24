suppressPackageStartupMessages({library(tidyverse)
library(lubridate)
library(stringi)
library(xml2)})

##################################################################
# CHANGE THESE TO CREATE A NEW CLASS
CLASS_NAME <- "DSST330: Statistics"
SEMESTER <- "Spring 2024"
ABB <- "dsst330-s24"
START_DATE <- "2024-01-15"

DATES <- c(
  "2024-01-15" = "No Class: MLK Day",
  "2024-01-17" = "[Introduction]",
  "2024-02-12" = "[Exam 01: Study Guide]",
  "2024-03-11" = "No Class: Spring Break",
  "2024-03-13" = "No Class: Spring Break",
  "2024-03-25" = "[Exam 02: Study Guide]",
  "2024-04-15" = "[Exam 03: Study Guide]",
  "2024-04-17" = "Workshop",
  "2024-04-22" = "[Final Project: Presentations]",
  "2024-04-24" = "[Final Project: Presentations]"
)

##################################################################
# SEMI-FIXED HEADER AND FOOTER FOR THE DEV.HTML PAGE
html_head <- '<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content="DSST 289">
    <meta name="author" content="Taylor Arnold">

    <title>%s</title>

    <link rel="stylesheet" href="css/reset.css">
    <link rel="stylesheet" href="css/style.css">
  </head>
  <body>
  <div id="page">
    <div id="header">
      <h1>%s &bull; %s &bull; Taylor Arnold</h1>
      <p>
        <a href="https://statsmaths.github.io/">[&lArr;]</a>
        <a href="extra/-syllabus.pdf">[syllabus]</a>
      </p>
    </div>
    <div id="grid">
      <table>
        <col style="width:12%%">
        <col style="width:6%%">
        <col style="width:35%%">
        <col style="width:16%%">
        <col style="width:16%%">
        <col style="width:16%%">
        <thead>
          <tr>
            <th></th>
            <th></th>
            <th><b>Handout</b></th>
            <th><b>Worksheet</b></th>
            <th><b>Solutions</b></th>
            <th><b>Notebook</b></th>
          </tr>
        </thead>'

html_tail <- '
      </table>
    </div>
    <span class="footer">Updated XX</span>
  </div>
  </body>
</html>'

##################################################################
# FILL IN HEADER
html_head_fill <- sprintf(html_head, CLASS_NAME, CLASS_NAME, SEMESTER)

##################################################################
# GET DATES
all <- sort(c(
  ymd(START_DATE) + 7 * seq(0, 14),
  ymd(START_DATE) + 7 * seq(0, 14) + 2
))

cnt <- cumsum(as.numeric(!(all %in% names(DATES))))
cnt[all %in% names(DATES)] <- 0

index <- match(all, names(DATES))
msg <- rep("", length(cnt))
msg[!is.na(index)] <- as.character(DATES)[index[!is.na(index)]]
msg_link <- (stri_sub(msg, 1L, 1L) == "[")

##################################################################
# CREATE TABLE ROWS
rows <- c()
for (j in seq_along(all))
{
  nr <- c('<tr>')

  # need to create an extra column if it is the first class of week
  if (wday(all[j], week_start = 1) %in% c(1L, 2L))
  {
    nr <- c(nr, sprintf('  <td rowspan="2"><b>%s</b></td>', all[j]))
  }
  nr <- c(nr, sprintf('  <td>%s.</td>', wday(all[j], label = TRUE, locale = "en_GB")))

  # if special class, fill across all days with the message/link;
  # otherwise, fill in the three columns
  if (cnt[j] == 0)
  {
    if (msg_link[j])
    {
      nr <- c(nr, sprintf('  <td colspan="4"><a class="%s ex" href="ex/">%s</a></td>', all[j], msg[j]))
    } else {
      nr <- c(nr, sprintf('  <td colspan="4">%s</td>', msg[j]))      
    }
  } else {
    nr <- c(nr, sprintf('  <td><a class="%s ho" href="ho/ho-%02d.pdf">[%02d.]</a></td>', all[j], cnt[j], cnt[j]))
    nr <- c(nr, sprintf('  <td><a class="%s ws" href="ws/ws-%02d.pdf">[ws-%02d]</a></td>', all[j], cnt[j], cnt[j]))
    nr <- c(nr, sprintf('  <td><a class="%s sl" href="sl/sl-%02d.pdf">[sl-%02d]</a></td>', all[j], cnt[j], cnt[j]))
    nr <- c(nr, sprintf('  <td><a class="%s nb" href="pg/pg-%02d.html">[nb-%02d]</a></td>', all[j], cnt[j], cnt[j]))
  }
  nr <- c(nr, '</tr>')

  rows <- c(rows, nr)
}

rows <- sprintf("        %s", rows)

##################################################################
# CREATE OUTPUT
write_lines(html_head_fill, "test.html")
write_lines(rows, "test.html", append = TRUE)
write_lines(html_tail, "test.html", append = TRUE)
