//shamelessly ripped off from @jimhester
#define USE_RINTERNALS
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP duplicate_(SEXP x) {
  return duplicate(x);
}
