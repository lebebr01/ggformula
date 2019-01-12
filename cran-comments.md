## Test environments

* local OS X install 
  * R version 3.5.2 (2018-12-20)
  * Platform: x86_64-apple-darwin15.6.0 (64-bit)
  * Running under: macOS Mojave 10.14.2

* win-builder via devtools::check_win_release()
* win-builder via devtools::check_win_devel()

* rhub via devtools::check_rhub() reports two notes:

    * one identifies the maintainer
    * the other reports some slightly long-running examples.  On the other platforms,
      these appear to run quickly enough.

## R CMD check results (local)

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded


## Downstream dependencies

Checked with `devtools::revdep_check()`.  

✔ fastR2 1.2.1                           ── E: 0     | W: 0     | N: 0                
✔ mosaic 1.4.0                           ── E: 0     | W: 0     | N: 0                
✔ mosaicCore 0.6.0                       ── E: 0     | W: 0     | N: 0                
✔ mosaicData 0.17.0                      ── E: 0     | W: 0     | N: 1                
✔ mosaicModel 0.3.0                      ── E: 0     | W: 0     | N: 1                
✔ supernova 1.1                          ── E: 0     | W: 0     | N: 0                
OK: 6                                                                               
BROKEN: 0

