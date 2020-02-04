(in-package #:info.isoraqathedh.bocproc.core)

(docs:define-docs
  (variable +bocproc-uuid+ "UUID to make page UUIDs out of.")

  (class stable-entity
    "Represents a stable entity. Refer to the README for more information.")

  (class affinity
    "Represents an affinity. Refer to the README for more information.")

  (class tag
    "Represents a tag. Refer to the README for more information.")

  (class page
    "Represents a page. Refer to the README for more information.")

  (function make-page-number-with-series
    "Create the first page number of the series given SERIES.

In general, if the date does not figure in the name of the series,
it is absolutely the first page defined.
If the date does show up in the page number,
then it would default to the first page of the current time period.")

  (function next
    "Retrieve the next page number after PAGE-NUMBER.

If SPEC is provided, then jump to the next page number
keeping all lower specificities the same.
Not providing SPEC is the same as specifying the last one.
Providing a SPEC that is not a defined specificity for the series is an error.

If the provided PAGE-NUMBER cannot be advanced,
then an error of `page-out-of-bounds' is signalled.")

  (function previous
    "As `next', but in the other direction.")

  (condition page-out-of-bounds-error
    "Indicates that the page number is out of bounds.")

  (function fill
    "Seek information of that page number from the data file.

This function will read the database for any matching page entries,
and then fill in the correct page information in a fresh object."))
