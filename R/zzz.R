.onAttach <- function(lib, pkg) {
  packageStartupMessage("VS Version ", packageVersion(pkg), " (beta version)\n",
                        "Please report any bugs to vs.quanpsy@gmail.com.")
}
