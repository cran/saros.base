## winget install Posit.rig
## #winget install Posit.RStudio
## # Restart cmd.exe
## #rig install R
## #rig system rtools add
## rig system setup-user-lib

## -----------------------------------------------------------------------------
install.packages(c("tidyverse", "devtools", "gt", "ggiraph", "webshot", "webshot2", "saros.base", "saros"))
webshot::install_phantomjs()
usethis::edit_r_environ()
# Add the following in the renviron-file being opened: LC_ALL="nb.utf8"
# Save file and restart RStudio

