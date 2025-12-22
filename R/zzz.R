# R/zzz.R
# Package initialization hooks

.onLoad <- function(libname, pkgname) {
  # Auto-register CS adapter if did package is available
  if (requireNamespace("did", quietly = TRUE)) {
    register_adapter(adapter_cs())
  }

  # Auto-register imputation adapter if didimputation package is available
  if (requireNamespace("didimputation", quietly = TRUE)) {
    register_adapter(adapter_imputation())
  }

  # Auto-register did2s adapter if did2s package is available
  if (requireNamespace("did2s", quietly = TRUE)) {
    register_adapter(adapter_did2s())
  }

  # Auto-register etwfe adapter if etwfe package is available
  if (requireNamespace("etwfe", quietly = TRUE)) {
    register_adapter(adapter_etwfe())
    register_adapter(adapter_etwfe_poisson())
  }
}
