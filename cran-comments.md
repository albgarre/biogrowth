## Resubmission
This is a resubmission with corrections according to the comments by Gregor Seyer on 2020/10/13 15:17. We appreciate the time spent by the reviewer checking our package. 

If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

> We acknowledge the comment. A reference to the book by Perez-Rodriguez and Valero (2012, ISBN:978-1-4614-5519-6)
has been included at the end of the description. Additionally, I have added my ORCID as recommended by CRAN.


Please do not modify the global environment (e.g. by using <<-) in your
functions. This is not allowed by the CRAN policies.

> We aknowledge the reviewer's comment. <<- was used to escape the scope of lapply in 
the functions extract_primary_pars and extract_secondary_pars. Both functions have been rewritten
avoiding the use of <<-.

## Test environments
* local R installation, R 3.5.3
* ubuntu 16.04 (on travis-ci), R 3.5.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
