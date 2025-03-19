

# ottrpal 2.0.0

- New check functions here! spell_check(), url_check(), quiz_check()
- borrow_chapter() with tag replacement functionality
- Better testing!
- OTTRfy your repositories
- Refactored code!

# ottrpal 1.2.1

* Fixed minor windows curl bug for CRAN compatibility

# ottrpal 1.2

* Has the ability to transfer notes from a Google slide to fill in the alternative text and captions for the images for `include_slide()`
* Can accommodate multiple css files https://github.com/jhudsl/ottrpal/pull/115

# ottrpal 1.0

* Attempt to fix citeproc error by @cansavvy in https://github.com/jhudsl/ottrpal/pull/7
* Undo citeproc change -- that wasn't the problem by @cansavvy in https://github.com/jhudsl/ottrpal/pull/9
* Get rid of setwd() bits by @cansavvy in https://github.com/jhudsl/ottrpal/pull/12
* Run Styler on the R files by @cansavvy in https://github.com/jhudsl/ottrpal/pull/15
* Fix youtube and embed links  by @cansavvy in https://github.com/jhudsl/ottrpal/pull/14
* Add footer function! by @cansavvy in https://github.com/jhudsl/ottrpal/pull/18
* Add commas and gha check by @cansavvy in https://github.com/jhudsl/ottrpal/pull/20
* Add footnote conversion: take 2 by @cansavvy in https://github.com/jhudsl/ottrpal/pull/24
* Try to fix GHA for filing PRs with leanbuild updates by @cansavvy in https://github.com/jhudsl/ottrpal/pull/27
* Functionalize youtube conversion of links by @cansavvy in https://github.com/jhudsl/ottrpal/pull/29
* Update some things on the GHA  by @cansavvy in https://github.com/jhudsl/ottrpal/pull/30
* Trim down dependencies by @cansavvy in https://github.com/jhudsl/ottrpal/pull/33
* Polish Book.txt workflow  by @cansavvy in https://github.com/jhudsl/ottrpal/pull/34
* Add a basic vignette! by @cansavvy in https://github.com/jhudsl/ottrpal/pull/36
* Polishing quiz checking by @cansavvy in https://github.com/jhudsl/ottrpal/pull/37
* Incorporate coursera functions by @cansavvy in https://github.com/jhudsl/ottrpal/pull/39
* Add output_yaml argument to render_coursera by @cansavvy in https://github.com/jhudsl/ottrpal/pull/42
* Make quiz checks more informative and user friendly by @cansavvy in https://github.com/jhudsl/ottrpal/pull/44
* Add a first vignette! by @cansavvy in https://github.com/jhudsl/ottrpal/pull/47
* Severing ties with didactr - Part II by @cansavvy in https://github.com/jhudsl/ottrpal/pull/51
* Add toc_close.css download by @cansavvy in https://github.com/jhudsl/ottrpal/pull/54
* Citeproc Strikes Back: Add warning and fix syntax by @cansavvy in https://github.com/jhudsl/ottrpal/pull/57
* I think this will remove the periods we need to get rid of by @carriewright11 in https://github.com/jhudsl/ottrpal/pull/69
* Add issue and PR templates! by @cansavvy in https://github.com/jhudsl/ottrpal/pull/70
* Fixing example file copy over function by @cansavvy in https://github.com/jhudsl/ottrpal/pull/72
* Get rid of pesky comma on image specs by @cansavvy in https://github.com/jhudsl/ottrpal/pull/74
* Add space before feedback linee by @cansavvy in https://github.com/jhudsl/ottrpal/pull/76
* Add nicely rendered docs using pkgdown by @cansavvy in https://github.com/jhudsl/ottrpal/pull/77
* Fixing a few minor docs formatting things  by @cansavvy in https://github.com/jhudsl/ottrpal/pull/78
* potential fix to copy quizzes to manuscript dir by @carriewright11 in https://github.com/jhudsl/ottrpal/pull/80
* Fix copy_quizzes to use `quiz_dir` by @cansavvy in https://github.com/jhudsl/ottrpal/pull/82
* Make so quiz_dir can be NULL by @cansavvy in https://github.com/jhudsl/ottrpal/pull/83
* Fix bookdown_to_book_txt by @cansavvy in https://github.com/jhudsl/ottrpal/pull/87
* Update links to OTTR by @cansavvy in https://github.com/jhudsl/ottrpal/pull/88
* Rename package to `ottr` by @cansavvy in https://github.com/jhudsl/ottrpal/pull/89
* Adding embed functionality for Leanpub conversion by @cansavvy in https://github.com/jhudsl/ottrpal/pull/90
* Improve Book_txt handling by @cansavvy in https://github.com/jhudsl/ottrpal/pull/92
* Add width/height argument options for iframe stuff by @cansavvy in https://github.com/jhudsl/ottrpal/pull/93
* Add footer_text functionality to ottr::bookdown_to_embed_leanpub by @cansavvy in https://github.com/jhudsl/ottrpal/pull/94
* Change render_coursera -> render_without_toc by @cansavvy in https://github.com/jhudsl/ottrpal/pull/96
* ottr -> ottrpal by @cansavvy in https://github.com/jhudsl/ottrpal/pull/98
* Update quiz handling by @cansavvy in https://github.com/jhudsl/ottrpal/pull/99
* Update vignettes and examples by @cansavvy in https://github.com/jhudsl/ottrpal/pull/100

# ottrpal 0.1.0

* Added a `NEWS.md` file to track changes to the package.
