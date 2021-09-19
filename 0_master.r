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

# Import libraries
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

# Load wrapper functions (Windows needs special treatment)
source(file = "1_os_compatibility_functions.r")

# Load minor functions
source.special(file = "2_minor_functions.r")

# Import distinctive features
source.special(file = "3_distinctive_features.r")

# Load English phonological information
source.special(file = "4_english_phonology.r")

# Load metrical parsing functions
source.special(file = "5_parsing_functions.r")

# Load dictionary functions
source.special(file = "6_dictionary_functions.r")
