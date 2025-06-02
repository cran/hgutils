# Build ---------------------------
.rs.restartR()
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:/Program Files/MiKTeX/miktex/bin/x64",
                        sep=.Platform$path.sep))
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)
setwd("C://Users//Windows//Dropbox//hgpackages//hgutils")
library(hgutils)
hgutils::startup()
# hgutils::crossref_description(skip_prompt=TRUE, use_version_numbers=FALSE,
#                               rversion="DEPENDENCIES_VERSION", update=TRUE)
#attachment::att_amend_desc()
devtools::document()
devtools::spell_check()
#devtools::run_examples(fresh=TRUE, run_dontrun = FALSE)
#devtools::test()

devtools::check() #    usethis,
update_description("Date", format(Sys.Date(), "%Y%-%m-%d"))
add_badges("hvdboorn/hgutils", show_travis = FALSE)

######### CHECK AS CRAN
#########
# Update dependencies in DESCRIPTION
# Check package as CRAN
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))

# Check content
# remotes::install_github("ThinkR-open/checkhelper")
checkhelper::find_missing_tags()

# Check spelling
# usethis::use_spell_check()
spelling::spell_check_package()

# Check URL are correct
# remotes::install_github("r-lib/urlchecker")
urlchecker::url_check()
urlchecker::url_update()

# check on other distributions
# _rhub
rhub::rc_submit()
#devtools::check_rhub()
# rhub::check_on_windows(check_args = "--force-multiarch")
# rhub::check_on_solaris()
# _win devel
devtools::check_win_devel()

# Check reverse dependencies
# remotes::install_github("r-lib/revdepcheck")
usethis::use_git_ignore("revdep/")
usethis::use_build_ignore("revdep/")

# devtools::revdep()
# library(revdepcheck)
# # In another session
# id <- rstudioapi::terminalExecute("Rscript -e 'revdepcheck::revdep_check(num_workers = 4)'")
# rstudioapi::terminalKill(id)
# # See outputs
# revdep_details(revdep = "pkg")
# revdep_summary()                 # table of results by package
# revdep_report() # in revdep/
# # Clean up when on CRAN
# revdep_reset()

# Update NEWS
# Bump version manually and add list of changes

# Add comments for CRAN
usethis::use_cran_comments(open = rlang::is_interactive())

# Verify you're ready for release, and release
devtools::release()
