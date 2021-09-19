# R-syllable-parser (Parse syllables using Phono 1 principles)
# Copyright (C) 2021 Jake W. Vincent <https://github.com/jakewvincent>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Identify OS
this_os <- .Platform$OS.type

##################################################################
# Modified source function for sourcing unicode files in Windows #
##################################################################

source.utf8 <- function(f) {
  l <- readLines(f, encoding = "UTF-8")
  eval(parse(text = l), envir = .GlobalEnv)
}

########################################
# Define wrapper function for source() #
########################################

source.special <- function(input) {
    if (this_os == "windows") {
        source.utf8(input)
    } else {
        source(input)
    }
}

####################################################################
# Modified print function to display unicode characters in Windows #
####################################################################

print.unicode_df <- function(x,
                             ...,
                             digits = NULL,
                             quote = FALSE,
                             right = TRUE,
                             row.names = TRUE) {
    n <- length(row.names(x))
    if (length(x) == 0L) {
        cat(sprintf(ngettext(n, "data frame with 0 columns and %d row",
                             "data frame with 0 columns and %d rows",
                             domain = "R-base"),
                    n),
            "\n",
            sep = "")
    }
    else if (n == 0L) {
        print.default(names(x),
                      quote = FALSE)
        cat(gettext("<0 rows> (or 0-length row.names)\n"))
    }
    else {
        m <- as.matrix(x)
        if (!isTRUE(row.names))
            dimnames(m)[[1L]] <- if (identical(row.names, FALSE))
                rep.int("", n)
            else row.names
            print(m, ..., quote = quote, right = right)
    }
    invisible(x)
}

#######################################
# Define wrapper function for print() #
#######################################

print.special <- function(input) {
    if (this_os == "windows") {
        print.unicode_df(input)
    } else {
        print(input)
    }
}
