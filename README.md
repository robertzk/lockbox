Painless Dependencies in R [![Build Status](https://img.shields.io/travis/robertzk/lockbox.svg)](https://travis-ci.org/robertzk/lockbox) ![Release Tag](https://img.shields.io/github/tag/robertzk/lockbox.svg) [![Documentation](https://img.shields.io/badge/rocco--docs-%E2%9C%93-blue.svg)](http://robertzk.github.io/lockbox/)
-------------

[Lockbox](https://screen.yahoo.com/gore-bush-first-debate-strategery-050000058.html) is
a very minimalistic [bundler](http://bundler.io/)-style dependency manager for R.

The goal of lockbox is to ensure you have to spend no time thinking about
package dependencies and get back to doing real work in R.

To get started, just write a lockfile.yml (see an example below) and execute

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

Real examples
------------

* [A very simple lockfile](https://github.com/syberia/base.sy/blob/master/lockfile.yml) 
* [A more complicated lockfile](https://github.com/syberia/example.sy/blob/master/lockfile.yml)

Example Lock File
-----------------

```yml
# lockfile.yml in the project you wish to dependency manage using lockbox
packages:
  -
    # This will install the tundra package from github's robertzk/tundra repo
    # off tag 0.2.2. You can specify a different tag / branch / ref using:
    #   ref: v0.2.2 # For example
    name: tundra
    version: 0.2.2
    repo: robertzk/tundra
  -
    name: director
    version: 0.2.1
    repo: robertzk/director
    load: false # This package will not be loaded by default. The user must
                # request it by calling library(director). In that case,
                # the correct version (0.2.1) will be loaded.
  -
    # Use the kselection package version 0.2.0 from CRAN.
    # If this is not the current version, the CRAN archive will be used.
    name: kselection
    version: 0.2.0
  -
    # Install a local (development) package version 0.1.0 from directory
    # /your/pkg. Using this approach is not recommended, since you will
    # not be able to keep your lock file under version control and shareable
    # with other contributors to the project, as they are unlikely to have
    # the same package / version in the same directory. However, it is useful
    # for development prior to pushing a new version to, e.g., github.
    name: yourpkg
    version: 0.1.0
    dir: /your/pkg
  -
    # Install a local package and re-install it every time your lockbox loads
    # regardless of package version, to make quick development possible.
    # ...Keep in mind that the contents of autoinstalled packages are only
    # *loaded*, and not re-saved to lockbox each time...
    name: yourpkg
    version: 0.1.0
    dir: /your/pkg
    autoinstall: true
  -
    # You can even install repos that are private on Github, as long as your
    # GIT_PAT environment variable is set (using your shell or, e.g., Sys.setenv)
    # to the value of your Github authorization token. To obtain one, see:
    #   https://help.github.com/articles/creating-an-access-token-for-command-line-use/
    # It should be a 32-character hexadecimal string.
    name: privatepkg
    version: 0.1.4
    repo: privateorg/privatepkg
```

Notes
-----

If you call `library(lockbox)` from within a directory that contains
a `lockfile.yml` somewhere in a parent directory, that lockfile
will be loaded and the relevant package versions attached to the 
search path. In particular, you can place `library(lockbox)` in
your `~/.Rprofile` and lockbox will be loaded if and only if
you start your R session from a project that is managed by lockbox.

This behavior can be disabled by setting `options(lockbox.autoload = FALSE)`.
