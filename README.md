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

Lambda list: `VERSION (&REST VERSION-NUMBERS)`

This is a vector that defines a version.
It is an identifier and does not affect processing directly.

Example:

    (version 6)

# Single file processing #

Lambda list:
`PROCESS-FILE
(FILE &KEY PAGING TITLE COMMENT TAGS EXPORT-TEXT ROTATE &ALLOW-OTHER-KEYS)`

Processes a single file.

Specifically, this function will order that FILE
will receive a new name as calculated by PAGING.
It will also receive EXIF tags as specified by COMMENT and TAGS.
Finally, CSV output will be directed to EXPORT-TEXT.

Explanation of each key option:

* `:PAGING (&REST PAGING-SPEC)`:
  Reserves a page number as understood by `PAGING-SPEC`,
  which in turn is either a plist of specificity keys
  or the symbol `:NEXT` followed by a specificity.
      * If it is a plist of specificities,
        then reserve the page number as specified by the specificities.
        For example, `(:PAGE 5 :SUBPAGE 6)` reserves that exact page number.
        Three special values can be used instead of a number:
          * `:FIRST`, a shorthand for the minimum possible value it can take,
          * `:NEXT`, the next available value, or
          * `:CUR`, one before `:NEXT`
      * If it is the symbol `:NEXT` followed by a specificity,
        it is a shorthand for a listing of all specificities,
        with everything before the listed specificity having value `:CUR`,
        everything after the listed specificity `:FIRST`,
        and the named specificity itself having the value `:NEXT`.
* `:TITLE TITLE`:
  Specifies the title string. Defaults to "untitled".
* `:COMMENT COMMENT`:
  Specifies a comment string. Defaults to NIL, representing no comment string.
* `:TAGS (&REST TAG-SPEC)`:
  Names a list of tags understood by their short names in the config file.

Example:

    (process-file "~/path/to/original/file.jpg"
      (:paging :next :page)
      (:title "Example title")
      (:comment "This is an example comment.")
      (:tags "EP" "SX" "RSGN")
      (:rotate 90)
      (:crop 15 15 -120 -120)
      (:tumblr t)
      (:export-text t))

Future options:

* `:EXPORT-TEXT EXPORT-PATHNAME`:

  *scheduled for version 6.1*

  Writes a CSV file to the designated pathname.
  This will, for each file, write the following values to its own line,
  in this order:
    * Page numbers, separated by pipes.
    * Title
    * Comment
    * An image URL, if `TUMBLR` is non-NIL.
* `:ROTATE ROTATE-SPEC`:

  *scheduled for version 6.1*

  The image must be rotated according to `ROTATE-SPEC`.
  `ROTATE-SPEC` must be one of the following:
      * No rotation: nil
      * 90° clockwise: 90, CW, R
      * 180°: 180, U
      * 90° anticlockwise: -90, 270, CCW, L
* `:CROP (TOP-LEFT-X TOP-LEFT-Y BOTTOM-RIGHT-X BOTTOM-RIGHT-Y)`:

  *scheduled for version 6.2*

  Specifies that the image should be cropped using the coordinates provided.
  All numbers should be specified in pixels, measured from the top-left corner
  However, if `BOTTOM-RIGHT-X` and  `BOTTOM-RIGHT-Y` are negative,
  then those two are specified from the bottom-right corner.
  Can be restricted to multiples of 8 or 16
  to ensure lossless rotation.
* `:TUMBLR POST-TO-TUMBLR-P`:

  *scheduled for version 7, if at all*

  Can only be nil or t. If t, posts to Tumblr via Humbler.
* `:IMGUR POST-TO-IMGUR-P`:

  *scheduled for version 7, if at all*

  Can only be nil or t.
  If t, posts the image to Imgur,
  potentially in an album if many are processed at the same time.

Sending posts to internet services is not at all easy,
and those options may not be implemented at all.

## Other commands ##

None of these commands are planned out into the program yet,
so they are considered "concepts".
A primary concern is that they require writing to the configuration file,
which is a nasty affair.

### Place a page number to the ignore list ###

Lambda-list: `IGNORE-PAGE (SERIES &REST PAGING-SPEC)`

Ignores a page permanently
by placing it into the ignore list in the configuration file.

### Add a tag ###

lambda-list: `ADD-TAG (TAG-SHORT-NAME &KEY NAME &ALLOW-OTHER-KEYS)`

Adds a tag to the configuration file.
