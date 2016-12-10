#' install_github installs R packages directly from GitHub
#'
#' install_github is a very lightweight function with no dependencies
#' that installs R packages from GitHub.
#'
#' @param repo identifies the GitHub repository, i.e. username/repo
#' @param branch identifies the branch
#' @param dependencies is a vector that indicates which type of additional
#'   packages are also to be installed. It can include any of the values in
#'   \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}. A
#'   value of \code{TRUE} corresponds to \code{c("Depends", "Imports",
#'   "LinkingTo")}. If \code{dependencies} equals \code{FALSE} no further
#'   packages will be installed.
#' @param method specifies which method \code{download.file} uses to download
#'   the zip archive from GitHub. Can be set to any of the values in
#'   \code{c("auto", "wget", "libcurl", "curl", "wininet")}. When set to
#'   \code{auto}, this function will make an educated guess.
#' @note For more information, visit: https://github.com/jtilly/install_github/
#' @examples
#' \dontrun{install_github("jtilly/matchingR")}
install_github = function(repo, branch = "master", dependencies = TRUE, method = "auto") {
  unix = Sys.info()["sysname"]=="Darwin" || Sys.info()["sysname"]=="Linux"
  if(method == "auto") {
    if(unix) {
      if(capabilities("libcurl")) {
        method = "libcurl"
      } else if (file.exists(Sys.which("wget"))) {
        method = "wget"
      } else if (file.exists(Sys.which("curl"))) {
        method = "curl"
      } else {
        stop("Could not find method to download files over https.")
      }
    } else {
      if(getRversion() < "3.2") {
        stop("Need R 3.2 or higher to download files over https.")
      }
      method = "wininet"
    }
  }

  url = paste0("https://github.com/", repo, "/archive/", branch, ".zip")
  tmpdir = tempdir()
  pkg.zip = file.path(tmpdir, paste0(branch, ".zip"))
  message(paste("Downloading", url, "using", method))
  if (method == "curl") {
    download.file(url = url, destfile = pkg.zip, method = method, extra = "-L")
  } else {
    download.file(url = url, destfile = pkg.zip, method = method)
  }
  message(paste("Saved as", pkg.zip))
  if (!file.exists(pkg.zip)) {
    stop("Download failed.")
  }
  pkg.root = file.path(tmpdir, branch)
  unzip(pkg.zip, overwrite = TRUE, exdir = pkg.root)
  message(paste("Extracted package to", pkg.root))
  pkg.dir = file.path(pkg.root, list.files(pkg.root)[1])
  if(dependencies == TRUE) {
    dependencies = c("Depends", "Imports", "LinkingTo")
  }
  if(!is.null(dependencies)) {
    # http://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran
    dcf = read.dcf(file.path(pkg.dir, "DESCRIPTION"))
    deps = unique(gsub("\\s.*", "", trimws(unlist(strsplit(dcf[, intersect(dependencies, colnames(dcf))], ","), use.names = FALSE))))
    deps = deps[!(deps %in% c("R", rownames(installed.packages())))]
    if(length(deps) > 0) {
      message(paste("Also installing:", paste(deps)))
      install.packages(deps)
    }
  }
  if(unix) {
    if(file.exists(file.path(pkg.dir, "configure"))) {
      Sys.chmod(file.path(pkg.dir, "configure"), mode = "0755", use_umask = TRUE)
    }
    if(file.exists(file.path(pkg.dir, "cleanup"))) {
      Sys.chmod(file.path(pkg.dir, "cleanup"), mode = "0755", use_umask = TRUE)
    }
  }
  install.packages(pkg.dir, repos = NULL, type = "source")
  unlink(pkg.root, recursive = TRUE, force = TRUE)
  unlink(pkg.zip, force = TRUE)
}
