# Book of Conworlds Processing System v. 6 #

This is the sixth generation of book of conworlds scan processing system,
which uses an s-expression-based system
to determine how individual image files
should be embedded with metadata and renamed.

# Introduction #

## Books of Conworlds ##

These books are collections of physical pieces of paper,
occasionally organised into literal books
that have to be scanned and digitalised.
They come in several **series**.
A series has a name, which is a short string that identifies it.
Currently there are four series:

* "boc" for the classic Book of Conworlds series.
* "purple" for the purple Book of Conworlds series
* "nboc" for pages relating to conworlding
  that isn't part of a Book of Conworlds series.
* "uncategorised" for all other pages.

## Specificities ##

Typically, pages doesn't just have one page number,
they have multiple page numbers.
For instance, a certain page might have page number `#(1 5 6)`,
which means that it is part 1, subpart 5, sub-subpart 6.

In order to identify them, each number also has a name attached to it.
This name is constant in a series, and is identified by a list of keywords.
For instance, the Book of Conworlds has specificities BOOK, PAGE and SUBPAGE.

All book series have at least PAGE, with various super- and subdivisions.

# Syntax #

## Version ##

Lambda list: `(&REST VERSION-NUMBERS)`

This is a vector that defines a version.
It is an identifier and does not affect processing directly.

Example:

    (version 6)

## Single file processing ##

Lambda list:
`(FILE &KEY PAGING TITLE COMMENT TAGS EXPORT-TEXT &ALLOW-OTHER-KEYS)`

Processes a single file.

Explanation of each key option:

* `:PAGING (&REST PAGING-SPEC)`:
  Reserves a page number as understood by `PAGING-SPEC`,
  which in turn is either a plist of specificity keys
  or the symbol `:NEXT` followed by a specificity.
  * If it is a plist of specificities,
    then reserve the page number as specified by the specificities.
    For example, `(:PAGE 5 :SUBPAGE 6)` is

Example:

    (process-file "~/path/to/original/file.jpg"
      :paging (:next :page)
      :title "Example title"
      :comment "This is an example comment."
      :tags ("EP" "SX" "RSGN")
      ;; :rotate 90
      ;; :crop (15 15 -120 -120)
      ;; :tumblr t
      :export-text t)
