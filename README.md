# onlineFDR Explore

## Introduction
Welcome to the onlineFDR Explore Shiny app. This repository contains an application that is an interface to the onlineFDR package hosted on Bioconductor. It is designed to assist researchers in controlling the False Discovery Rate (FDR) in an online manner. The app provides a family of algorithms that users can use, a tool to help decide which one to use, and several features including plotting the adjusted significance thresholds against a Bonferroni correction as well as the compare tool to compare two algorithms against each other. Users are also able to download the results. The [Explore](https://mrc-bsu.shinyapps.io/onlineFDRExplore/) version of the app is meant for post-hoc use.

## Who Should Use this App
Anyone who's looking to get a sense of how FDR control in an online manner would look like in an academic or industry context can benefit from this app. Generally, you'll be someone who conducts lots of hypothesis tests. If you're a student learning about FDR control, this app also aims to be an educational tool.

## Background behind algorithms in this app
Online FDR control refers to controlling the multiple hypothesis testing problem when hypotheses arrive sequentially in a stream. The way these algorithms work essentially relies on a concept called "alpha wealth" in which it costs "wealth" to conduct experiments but making rejections earn back wealth.  
For more information, please visit our [vignette](https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html)!
