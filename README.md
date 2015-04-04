Dependency Management for R [![Build Status](https://travis-ci.org/robertzk/lockbox.svg?branch=master)](https://travis-ci.org/robertzk/lockbox) [![Coverage Status](https://coveralls.io/repos/robertzk/lockbox/badge.svg?branch=master)](https://coveralls.io/r/robertzk/lockbox)
===========

[Lockbox](https://screen.yahoo.com/gore-bush-first-debate-strategery-050000058.html) is
a very minimalistic [bundler](http://bundler.io/)-style dependency manager for R.

The main difference between lockbox and more heavy-duty approaches like [Packrat](https://github.com/rstudio/packrat)
is that lockbox believes you should spend a specific amount of time
thinking about dependencies: none.

To get started, just write a lockfile.yml (TODO: explain format) and execute

```r
lockbox::lockbox("lockfile.yml")
```

Your search path will be cleared of all other loaded packages, ensuring you
have exactly and only what is in the lockfile. If you place the above line
in the `.Rprofile` accompanying your project directory and keep the 
`lockfile.yml` under source control, launching the R console after
switching to earlier commits will instantly load your old dependencies,
forever freeing you from having to worry about dependency management.

Installation
------------

This package is not yet available from CRAN (as of April 4, 2015).
To install the latest development builds directly from GitHub, run this instead:

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("robertzk/lockbox")
```





