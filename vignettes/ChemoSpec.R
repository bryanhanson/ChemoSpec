## ----SetUp, echo = FALSE, eval = TRUE, results = "hide"----

# You must knit this file with getwd() set to the directory it is in!

# R options & configuration:

rm(list = ls())
options(width =  50, show.signif.stars = FALSE)

suppressMessages(library("knitr"))
suppressMessages(library("ChemoSpec"))
suppressMessages(library("RColorBrewer"))
suppressMessages(library("mvbutils"))
suppressMessages(library("sna"))

desc <- packageDescription("ChemoSpec")
vers <- paste("(Package Version ", desc$Version, ")", sep = "")
#vers <- paste("R package version", meta$Version)

# Stuff specifically for knitr:

opts_chunk$set(out.width = "0.8\\textwidth", fig.align = "center", fig.width = 7, fig.height = 7, cache = FALSE)

# Note: defaults are eval = TRUE, echo = TRUE

## ----Chunk1,  results = "hide", eval = FALSE----
#  source("My_First_ChemoSpec.R")

## ----Chunk2,  results = "hide", eval = FALSE----
#  files2SpectraObject(gr.crit = c("sspA", "sspB"), gr.cols = c("red3", "dodgerblue4"),
#  freq.unit = "ppm", int.unit = "peak intensity", descrip = "Subspecies Study",
#  out.file = "subspecies")

## ----Chunk3,  results = "hide", eval = FALSE----
#  SubspeciesNMR <- loadObject("subspecies.RData")

## ----Chunk5, echo = TRUE------------------------
data(SrE.IR) # makes the data available
sumSpectra(SrE.IR)

