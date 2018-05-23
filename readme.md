# Stacked Generalization for Model Based Geostatistics

## Overview

Stacked generalization is a method of ensembling machine learning algorithms and/or statisical models such that the combination of a collection of children models performs better on predictive validity statistics than any individual component. In this implementation, a two stage approach where a series of first stage (child) models are fit and then are ensembled in a parent geostatistical model is assumed as the default workflow. It is also assumed that the user is a friend of IHME (although all are welcome) and has access to high performance computing. Specifically, the packake is designed to take advantange of IHME's cluster computing infrastructure.

This work was inspired by Bhatt et al. (http://rsif.royalsocietypublishing.org/content/14/134/20170520.figures-only) and further details on the "why?" can be found there.


## How To Install

```devtools::install_github('dahcase/mbgstacking')```

## Getting Started

## Tests

## General Tips
