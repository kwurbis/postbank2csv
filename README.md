# postbank2csv

Commandline program to convert bank statements from the German Postbank from PDF to CSV format (for further input in (h)ledger).

```
$ postbank2csv <dir or pdf file>
```

Creates a file named after the date range of the statement (example "20200204_20200306.csv") next to each input pdf.


## Installation on MacOS
- Install Haskell using the instructions on the following site: https://www.haskell.org/ghcup/
- git clone this repository
- cd into the cloned repository
- build the postbank2csv executable using
```
$ stack build
```
- install pdftotext using brew, which seems to be containd in the package poppler

```
$ brew install poppler
```






