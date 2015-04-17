# Version 0.1.9

  * For packages installed from github, the `subdir` parameter in the lock file
    will be passed to `devtools::install_github` during installation.

  * Added the `load` option to elements in the lock file. If set to `false`, 
    the package will not be loaded by default, but will have to be requested
    by the user using `library(packagename)`.

# Version 0.1.8

  * If the user sets the `GIT_PAT` system environment variable to their Github
    personal authorization token, private Github repos will be manageable by lockbox.
    Check out [Github's guide](https://help.github.com/articles/creating-an-access-token-for-command-line-use/)
    to understand how to obtain a personal authorization token. It should be a 
    32 character hexadecimal string.

# Version 0.1.7
  
  * Using the `base::addTaskCallback` mechanism, lockbox moves any installed
    packages from the transient library to the user's "real" library if
    they installed packages in a session that was loaded using lockbox.

    This is to ensure that no packages are installed into the transient library
    or the lockbox library unless they are explicitly in a lockfile.

# Version 0.1.5

  * Installing non-github (i.e. CRAN) packages now works on Ubuntu.

# Version 0.1.4

  * Support for installing from local packages by setting `remote: local`
    and `dir: /your/pkg/dir`.

# Version 0.1.3

  * Fixed a bug where empty libraries were not correctly symlinked during staging
    library creation and instead triggered an error.

# Version 0.1.2

  * [Thanks](https://github.com/robertzk/lockbox/pull/6) to [Kirill Sevastyanenko](https://github.com/kirillseva),
    support for downloaded older versions of packages from CRAN was added. #15

# Version 0.1.1

  * Added a vignette "Libraries in Lockbox" explaining the three different libraries
    that make lockbox works: the lockbox library, the transient library, and the
    staging library.

  * Added internal set of functions in `staging.R` to create a virtual library
    that is used when installing new packages to lockbox. Prior to this,
    installation of new packages that had a non-zero number of dependencies
    failed.

# Version 0.1.0

  * The initial creation of the package. 
