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
For instance, a certain page might have page number `(1 5 6)`,
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

<!--
# Verbosity #

Lambda list: `LOUD (&OPTIONAL ON-OFF)`

Toggles the display of destination information.

The parameter `ON-OFF` controls whether or not verbosity is turned on or not.
If it is not supplied, then it toggles verbosity.
Otherwise, it is turned off if `nil`, and on if non-`nil`.

Additionally, if verbosity is turned on,
execution is paused for two seconds at the end to allow reading of the output.
-->

# Single file processing #

Lambda list: `PROCESS-FILE (FILE &REST OPTIONS)`

Systematically alters the contents (viz. cropping and rotating),
metadata and eventually the location of a single file, named by `FILE`.

`OPTIONS` is a list of lists,
with the first element of each list being an option keyword
and the rest being that option's arguments.

Currently there are the following options.

* `(:PAGING SERIES &REST PAGING-SPEC)`:
  Reserves, and eventually ensures that the output file
  will have a file name corresponding to the page number
  as understood by the combination of `SERIES` and `PAGING-SPEC`,
  which in turn is either a plist of specificity keys
  or the symbol `:NEXT` followed by a specificity.
  * If it is a plist of specificities,
    then reserve the page number as specified by the specificities.
    For example, `("boc" :PAGE 5 :SUBPAGE 6)` reserves that exact page number
    in the "boc" series.
    Three special values can be used instead of a number:
    * `:FIRST`, a shorthand for the minimum possible value it can take,
    * `:NEXT`, the next available value, or
    * `:CUR`, one before `:NEXT`
  * If it is the symbol `:NEXT` followed by a specificity,
    it is a shorthand for a listing of all specificities,
    with everything before the listed specificity having value `:CUR`,
    everything after the listed specificity `:FIRST`,
    and the named specificity itself having the value `:NEXT`.
* `(:TITLE TITLE)`:
  Specifies the title string. Defaults to "untitled".
  Ensures that the `Title` metadata on the file is set to that value,
  if non-nil.
  Existing metadata might be overwritten.
* `(:COMMENT COMMENT)`:
  Specifies a comment string. Defaults to NIL, representing no comment string.
  Ensures that the `Comment` metadata field on the file is set to that value,
  if non-nil.
  Existing metadata might be overwritten.
* `(:TAGS &REST TAG-SPEC)`:
  Names a list of tags understood by their short names in the config file.
  Ensures that the `Subject` metadata field on the file is set to those values.
  Existing metadata might be overwritten.
* `(:TUMBLR TUMBLR-P)`
  Determines whether or not this particular image should be posted to Tumblr.
  Tumblr-p should be either T (for posting) or NIL (for not-posting).
  In the future this may accept other symbols
  that allow for the creation of photo sets.
  
  Posting will use the `:COMMENT` and `:TAGS` field as well,
  to provide the caption and the tags respectively.
  
  If this option is enabled,
  additionally dump the URLs that the copies live in to a file indicated by the config option
  `:dump-file` in a special (idiosyncratic) format.

Example:

    (process-file "~/path/to/original/file.jpg"
      (:paging "boc" :next :page)
      (:title "Example title")
      (:comment "This is an example comment.")
      (:tags egonyota-pasaru serkafph-xaxex road-signs)
      (:rotate 90)
      (:crop 15 15 -120 -120)
      (:tumblr t))

Future options:

* `(:ROTATE ROTATE-SPEC)`:

  The image must be rotated according to `ROTATE-SPEC`.
  `ROTATE-SPEC` must be one of the following:
      * No rotation: nil
      * 90° clockwise: 90, CW, R
      * 180°: 180, U
      * 90° anticlockwise: -90, 270, CCW, L
* `(:CROP TOP-LEFT-X TOP-LEFT-Y BOTTOM-RIGHT-X BOTTOM-RIGHT-Y)`:

  Specifies that the image should be cropped using the coordinates provided.
  All numbers should be specified in pixels, measured from the top-left corner
  However, if `BOTTOM-RIGHT-X` and  `BOTTOM-RIGHT-Y` are negative,
  then those two are specified from the bottom-right corner.
  Can be restricted to multiples of 8 or 16
  to ensure lossless rotation.
  
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

### Multiple posting ###

Lambda-list `POST-TO-TUMBLR (&OPTIONAL COMMENT &REST NAMES)`

