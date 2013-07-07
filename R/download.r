
# Downloading should occur in a session specific temporary directory, so
# that if installation fails for some reason, you can pick up where you
# left off without have to redownload many packages.
#
# Best way of downloading would be to simultaneously download using RCurl.
# Ideally, could do progress bar that accurately reflects total overall progress
# of downloading the bytes
#
# Partial downloads should be cleaned up - only successful downloads should
# work. May want to consider automatic checksumming if that's available from
# R.
download_packages <- function(package, repos, type) {



  # make urls
  # remove already downloaded files
  # return vector of paths

}

