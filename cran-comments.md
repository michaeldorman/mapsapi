## Test environments
* local Ubuntu 16.04 install, R 3.4.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE (New Submission).

## Downstream dependencies
None.

## Resubmission
This is a resubmission. In this version I have:

* Written software names in single quotes in title and description ('Google Maps').

* Small examples *outside* of '\dontrun{}' added for all functions. These use sample API responses included with the package, to avoid API rate limits and connection problems in package tests. Examples *inside* '\dontrun{}' do include API calls too.
