# Stacked Generalization for Model Based Geostatistics

## Overview

Stacked generalization is a method of ensembling machine learning algorithms and/or statisical models such that the combination of a collection of children models performs better on predictive validity statistics than any individual component. In this implementation, a two stage approach where a series of first stage (child) models are fit and then are ensembled in a parent geostatistical model is assumed as the default workflow. It is also assumed that the user is a friend of IHME (although all are welcome) and has access to high performance computing. Specifically, the package is designed to take advantange of IHME's cluster computing infrastructure.

This work was inspired by Bhatt et al. (http://rsif.royalsocietypublishing.org/content/14/134/20170520.figures-only) and further details on the "why?" can be found there.


## How To Install

```devtools::install_github('dahcase/mbgstacking')```

## Getting Started

## Tests

At the moment, there is no strong unit-testing framework. There are a few all-up tests that are run informally after a new version release, but nothing systematic yet. This is because I didn't really know what unit-testing was when I was revising things. What a nieve youngin I was. Unit tests will be implemented once development re-resumes (in the next month or so). I will also be exploring formal class systems (e.g. S4 or R6) to handle the initialization, dispatch, and collection of models.

## General Tips
