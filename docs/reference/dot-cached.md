# A simple pass-through filesystem cache for complex or long running operations

executes `.expr` and saves the output as an RDS file indexed by hash of
code in `.expr` and the hash of specified input variables (in `...`,
which should contain any inputs that influence `.expr`). The expression
is evaluated in the current environment and it is up to the user to
ensure all significant environmental factors are accounted for in `...`

## Usage

``` r
.cached(
  .expr,
  ...,
  .nocache = getOption("cache.disable", default = FALSE),
  .cache = .cache_loc(),
  .prefix = "cached",
  .stale = Inf
)
```

## Arguments

- .expr:

  the code the output of which requires caching. Other than a return
  value this should not create side effects or change global variables.

- ...:

  inputs that the code in `.expr` depends on and changes in which
  require the code re-running.

- .nocache:

  an option to defeat the cacheing which can be set globally as
  `options("cache.disable"=TRUE)`

- .cache:

  the location of the cache as a directory. May get its value from
  `getOption("cache.path")` or the default value which is
  `rappdirs::user_cache_dir(<package_name>)`

- .prefix:

  (optional) a name of the operation so that you can namespace the
  cached files and do selective clean up operations on them with
  .cache_clear()

- .stale:

  the length of time in days before considering cached data as stale.

## Value

the output of `.expr` which will usually be a value

## Unit tests



    # we use `rnorm` here to prove that the cache is working but in reality
    # you would normally only use something deterministic in `.expr`:
    x = 100
    tmp = .cached(stats::rnorm(x), x)
    tmp2 = .cached(stats::rnorm(x), x)
    testthat::expect_equal(tmp, tmp2)

    x = 200
    tmp3 = .cached(stats::rnorm(x), x)
    testthat::expect_equal(x, length(tmp3))
