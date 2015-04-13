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
