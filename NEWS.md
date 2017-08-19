# Version 0.2.5.1

  * Version numbers with two digits (e.g., "0.2") are now cast as character.

# Version 0.2.5.0

  * Stores and restores github packages specified by ref by hash.  They are only used if specified in the lockfile.

# Version 0.2.4.8

  * Fix printing of dependencies loaded during `lockbox::lockbox` call.

# Version 0.2.4.7

  * Fix the bug surfaced by https://github.com/robertzk/lockbox/issues/116.

# Version 0.2.4.6

  * Explicitly reference `methods::is`.

# Version 0.2.4.5

  * Add session identifiers to staging library.

# Version 0.2.4.4

  * Remove unnecessary package exports (`setNames`, `packageVersion`, `untar`).

# Version 0.2.4.3

  * Local remote can be inferred from the presence of a `dir` key.

# Version 0.2.4.1-2

  * Fix a bug for autoinstall.

# Version 0.2.4

  * Do not re-download packages during dependency downloading/parsing phase.
  * Use order in lockfile for namespace loading.

# Version 0.2.3

  * Adds an option for `autoinstall: true` for quick development on local packages.

# Version 0.2.2

  * Fix regex to handle multiple remotes and be more flexible on CRAN's available packages filter.

# Version 0.2.1

  * Ignore locks from parent directories when creating the staging library.

# Version 0.2.0

  * Support recursive downloading of CRAN dependencies.

# Version 0.1.12

  * Use a path for the staging library that is always available
    from a permissions perspective. (`tempdir()` may not always be
    writable)

# Version 0.1.11

  * Fixed help files by moving storage of all packages to `pkg_name/version/pkg_name`.

# Version 0.1.10

  * Added the notion of a transient staging library. Previously, installing packages
    while lockboxed would conflict with the transient library, since one may overwrite
    the symlinks in the transient library. Instead of attaching one extra library path
    (the transient library), we now attach *two*: the other is the *transient
    staging library*, which is simply an empty directory used in conjunction with
    a [task callback](https://stat.ethz.ch/R-manual/R-devel/library/base/html/taskCallbackManager.html)
    to transfer interactively installed packages to the vanilla library.

    The location of the transient staging library is fixed to be the
    same as the transient library suffixed with `_staging`.

# Version 0.1.9.6

  * Prettified the output of the installation and made it look more bundler like.
    This comes at a price of hiding the installation log output. If you want
    to debug some issue with lockbox make sure to set `options(lockbox.autoload = TRUE)`.

# Version 0.1.9.5

  * Added a `lockbox.default` option which, when set to the path of a
    lockfile.yml, will load that lockfile after attaching the package
    to the search path (note this behavior is disabled if
    `options(lockbox.autoload = FALSE)`).

# Version 0.1.9.2-4

  * Assorted fixes to library loading.

  * When lockbox is attached to the search path while the current directory
    is a project managed by lockbox (i.e., has a lockfile.yml somewhere in
    a parent directory), the relevant package versions will be autoloaded.
    This behavior can be disabled with `options(lockbox.autoload = FALSE)`.

  * The option `lockbox.env` for controlling the default environment to
    load when calling `lockbox::lockbox`.

# Version 0.1.9.1

  * To be more consistent with `devtools`, you now have to set GITHUB_PAT
    environment variable to install private repos from github.

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
