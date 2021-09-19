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

# The diphthongs that exist in English
english_diphthongs <- c("aʊ", "aɪ", "eɪ", "oʊ", "ɔɪ")

# Licit onsets
english_onsets <- c("ʃɹ", "θɹ", "bɹ", "bj", "bl", "dɹ", "dw", "fɹ", "fj",
                    "fl", "gɹ", "gl", "gw", "hj", "hw", "kɹ", "kj", "kl",
                    "kw", "mj", "pɹ", "pj", "pl", "sk", "sl", "sm", "sn",
                    "sp", "st", "sw", "tɹ", "tw")

# TODO: Make a vector for *exceptional* onsets

# These segments can't appear in onsets at all
english_banned_onsets <- c("ŋ")

# For calculating sonority profiles, are there any consonants that are
# honorarily a member of a different sonority class?
english_sonority_exceptions <- c(NULL)

# The sonority difference between a segment and a preceding consonant must be
# greater than or equal to this number
english_son_diff <- 1
