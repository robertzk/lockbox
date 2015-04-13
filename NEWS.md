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
