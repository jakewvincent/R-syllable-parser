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

##############################################
# Make a list each for consonants and vowels #
##############################################
features %>%
    subset(syllabic == 1, segment, 1) %>%
    paste0(collapse = "|") -> vowels

features %>%
    subset(syllabic != 1, segment, 1) %>%
    paste0(collapse = "|") -> consonants

#############################################################
# Translate a phonetic transcription into Cs and Vs (lossy) #
#############################################################
cvify <- function(str) {
    # If a character is syllabic, V. Otherwise, C.
    str %>% gsub(pattern = consonants,
                 replacement = "C") %>%
        gsub(pattern = vowels,
             replacement = "V") -> str_cvified
    # Return the string
    return(str_cvified)
}

###################################################################
# Translate a phonetic transcription into sonority levels (lossy) #
###################################################################
sonority <- function(str,
                     split = FALSE) {
    # Split the string into multiple character vectors
    str_split <- unlist(strsplit(str, ""))
    # Make a copy to work with
    str_son <- str_split
    # For each character, replace it with its sonority value
    for (char in seq(str_split)) {
        str_son[char] <- features$sonority[features$segment == str_split[char]]
    }
    # Collapse back into a single string
    str_son <- paste0(str_son, collapse = "")
    if (split == TRUE) {
        str_son <- unlist(strsplit(str_son, split = ""))
    }
    # Return the string
    return(str_son)
}

#####################################################
# Say whether a string of two vowels is a diphthong #
#####################################################
is_diphthong <- function(str, diphthongs = en_diphthongs) {
    # See if the string is in the list of diphthongs
    value <- str %in% diphthongs
    return(value)
}

######################################
# Find likely diphthongs in a string #
######################################
find_diphthongs <- function(str, diphthongs = en_diphthongs) {
    # Turn into CV string
    cv_structure <- cvify(str)
    # See if there are any two vowels next to ea other
    vv_matches <- gregexpr("VV", cv_structure)

    # If there was a match, add the text of the matches to a vector
    vv_text <- NULL
    if (vv_matches[[1]][1] > -1) {
        for (match in seq(length(vv_matches[[1]]))) {
            p <- vv_matches[[1]][match]
            text <- paste0(unlist(strsplit(str, ""))[p:(p + 1)], collapse = "")
            vv_text <- c(vv_text, text)
        }
    }

    # Get a list ready
    the_diphthongs <- list()

    # For each known diphthong (if any), save its text and position in a list
    if (TRUE %in% is_diphthong(vv_text)) {
        # Which VV sequences are diphthongs?
        which_vvs <- which(is_diphthong(vv_text, diphthongs = diphthongs))
        for (d in seq(which_vvs)) {
            # Add the text to the text part of the list
            the_diphthongs$text[d] <- vv_text[which_vvs[d]]
            # Add the position to the position part of the list
            the_diphthongs$positions[d] <- vv_matches[[1]][d]
        }
    } else {
        the_diphthongs <- NULL
    }

    # Return the list
    return(the_diphthongs)
}

####################################
# Find syllable nuclei in a string #
####################################
assign_nuclei <- function(str,
                          diphthongs = en_diphthongs,
                          verbose = FALSE) {
    # Make list w/ string & CV structure
    syllables <- list(word = str,
                      split_string = unlist(strsplit(str, "")),
                      cv_structure = unlist(strsplit(cvify(str), "")),
                      sonority_profile = unlist(strsplit(sonority(str), "")),
                      n_syllables = 0,
                      syllable_ids = unlist(strsplit(cvify(str), "")),
                      syllable_text = NULL,
                      nucleus_text = NULL,
                      nucleus_positions = NULL)

    # Look for nuclei, front to back
    for (segment in seq(length(syllables$split_string))) {
        # Check if current segment is a V that is unparsed
        if (syllables$cv_structure[segment] == "V" &
            syllables$syllable_ids[segment] == "V") {
            # If a V, check if it's not word-final
            if (segment < max(seq(length(syllables$split_string)))) {
                # If it isn't word-final, check if next segment is V
                if (syllables$cv_structure[segment + 1] == "V") {
                    # If next segment is V, see if the two Vs make a diphthong
                    maybe_diphthong <- paste0(syllables$split_string[segment],
                                              syllables$split_string[segment + 1],
                                              collapse = "")
                    if (is_diphthong(maybe_diphthong, diphthongs)) {
                        # Add 1 to counter--we're working with a new nucleus
                        syllables$n_syllables <- syllables$n_syllables + 1
                        # Add the text of the diphthong to a single slot
                        syllables$nucleus_text[syllables$n_syllables] <- maybe_diphthong
                        # In syllable_ids (copy of cv_structure), swap the Vs w/
                        # the nucleus number
                        syllables$syllable_ids[segment] <- syllables$n_syllables
                        syllables$syllable_ids[segment + 1] <- syllables$n_syllables
                        # Record the position of the nucleus
                        syllables$nucleus_positions[syllables$n_syllables] <- segment
                    # If the next seg IS a V but NOT a diphthong, just use
                    # the one V as a nucleus
                    } else {
                        # Add 1 to counter--we're working with a new nucleus
                        syllables$n_syllables <- syllables$n_syllables + 1
                        # Add the text of the vowel to a new slot
                        syllables$nucleus_text[syllables$n_syllables] <- syllables$split_string[segment]
                        # In syllable_ids, swap the V w/ the nucleus number
                        syllables$syllable_ids[segment] <- syllables$n_syllables
                        # Record the position of the nucleus
                        syllables$nucleus_positions[syllables$n_syllables] <- segment
                    }
                # If the next segment is NOT a V, ...
                } else {
                    # Add 1 to counter--we're working with a new nucleus
                    syllables$n_syllables <- syllables$n_syllables + 1
                    # Add the text of the vowel to a new slot
                    syllables$nucleus_text[syllables$n_syllables] <- syllables$split_string[segment]
                    # In syllable_ids, swap the V w/ the nucleus number
                    syllables$syllable_ids[segment] <- syllables$n_syllables
                    # Record the position of the nucleus
                    syllables$nucleus_positions[syllables$n_syllables] <- segment
                }
            # If it IS word-final, ...
            } else {
                # Add 1 to counter--we're working with a new nucleus
                syllables$n_syllables <- syllables$n_syllables + 1
                # Add the text of the vowel to a new slot
                syllables$nucleus_text[syllables$n_syllables] <- syllables$split_string[segment]
                # In syllable_ids, swap the V w/ the nucleus number
                syllables$syllable_ids[segment] <- syllables$n_syllables
                # Record the position of the nucleus
                syllables$nucleus_positions[syllables$n_syllables] <- segment
            }
        # If current segment is a C or is an already-parsed V, do nothing
        }
        # If verbose option is TRUE, print the list after each loop
        if (verbose) {
            cat("####################\n")
            cat(paste0("LOOP #", segment, ":\n"))
            cat("####################\n")
            print.special(syllables)
        }
    }

    # Now that nucleus text is ID'd, use the same values for syllable text
    syllables$syllable_text <- syllables$nucleus_text
    # Return list
    return(syllables)
}

###############################################################
# How many Cs are there immediately preceding given position? #
###############################################################
find_preceding_cs <- function(cv, position) {
    # Where are the Cs in the string?
    c_positions <- which(cv == "C")
    # Make a vector to store the positions of the preceding consonants
    preceding_cs <- NULL
    # Repeatedly check if any C positions immediately before provided position
    not_done <- TRUE
    previous_position <- position - 1
    while (not_done) {
        if (previous_position %in% c_positions) {
            preceding_cs <- c(c_positions[which(c_positions == previous_position)],
                              preceding_cs)
            previous_position <- previous_position - 1
        # If that condition isn't met, we're done and should stop
        } else {
            not_done <- FALSE
        }
    }

    # Return a vector with the positions of the preceding Cs
    return(preceding_cs)
}

#################################################
# Evaluate sonority profile for 2-segment onset #
#################################################
good_sonority_profile <- function(sonority_profile,
                                  min_son_diff = en_min_son_diff,
                                  verbose = FALSE) {
    # We need a multi-element vector, so if everything is crammed together,
    # split it up
    if (length(sonority_profile == 1) & sum(nchar(sonority_profile))) {
        sonority_profile <- unlist(strsplit(sonority_profile, split = ""))
        if (verbose) warning("The provided string had to be split.")
    }
    # If the input contains no digits, they must be the segments in a
    # transcription and we need to get their sonority
    if (unique(grepl("\\d", sonority_profile)) != TRUE) {
        sonority_profile <- sonority(sonority_profile, split = TRUE)
        if (verbose) warning("Segments were provided and had to be converted to a sonority profile.")
    }
    # Only do the check if the provided profile is longer than 1 element;
    # otherwise, it will be a vacuously good profile
    if (length(sonority_profile) > 1) {
        # Look at the next value and get the difference
        son_diff <- as.numeric(sonority_profile[2]) - as.numeric(sonority_profile[1])
        if (son_diff >= min_son_diff) {
            # The profile is good
            if (verbose) warning(paste0("Sonority difference = ", son_diff))
            is_good <- TRUE
        } else {
            is_good <- FALSE
        }
        # If we are at the end of the sequence, do nothing. Is_good is TRUE
        # by default, so if the length of the profile is 1, it is a vacuously
        # good sonority profile
    # If the profile is only 1 value long, the profile is vacuously good
    } else {
        is_good <- TRUE
    }
    # Return the goodness
    return(is_good)
}

###########################
# Assign onsets to nuclei #
###########################
assign_onsets <- function(list,
                          exceptional_onsets = en_exceptional_onsets,
                          banned_onsets = en_banned_onsets,
                          verbose = FALSE,
                          min_son_diff = en_min_son_diff) {
    # Add some elements to the list
    list$onset_text <- NULL
    list$onset_positions <- NULL

    # Make sure the input is compatible w/ what the function needs
    if (class(list) == "character") {
        stop("The input to this function must be the output of assign_nuclei()")
    } else {
        # Call the provided list "syllables"
        syllables <- list
        # How many nuclei are there?
        seq_nuclei <- seq(length(syllables$nucleus_positions))
        # Make index for going BACKWARDS through the nuclei
        current_nucleus <- max(seq_nuclei)

        # For each nucleus, assign the Cs before it as onsets (if possible)
        for (nucleus in seq_nuclei) {

            # Check that the nucleus is NOT word-initial
            if (syllables$nucleus_positions[current_nucleus] != 1) {

                # What are the positions of the Cs to consider for onset of this nucleus (if any)?
                preceding_cs <- find_preceding_cs(syllables$syllable_ids,
                                                  syllables$nucleus_positions[current_nucleus])

                # How many preceding Cs are there?
                seq_preceding_cs <- seq(length(preceding_cs))
                current_consonant <- max(seq_preceding_cs)
                # Make a vector for the onset
                onset <- NULL

                # For each C before nucleus, make it an tentative onset & see what happens
                for (c in seq_preceding_cs) {
                    # Grab the character and tentatively stick it into the onset
                    # On the first run of the loop, it will be stuck onto NULL,
                    # which will make it an onset of length 1
                    maybe_onset <- paste0(c(syllables$split_string[preceding_cs[current_consonant]],
                                            onset),
                                          collapse = "")

                    # Make sure the tenative onset isn't in the list of banned onsets
                    if (!(maybe_onset %in% banned_onsets)) {

                        # If not banned, consider how good the sonority profile is or whether the onset is exceptional
                        sonority_profile_good <- good_sonority_profile(maybe_onset,
                                                                       min_son_diff)
                        if (sonority_profile_good | maybe_onset %in% exceptional_onsets) {

                            # If verbose is TRUE, print some details about why parsing succeeded
                            if (verbose) {
                                if (sonority_profile_good) {
                                    warning(paste0("'", maybe_onset, "' has a good sonority profile."))
                                } else {
                                    warning(paste0("'", maybe_onset, "' is an exceptional onset."))
                                }
                            }

                            # If it's good, keep the change to the onset!
                            onset <- maybe_onset

                            # Mark that C as parsed into the current nucleus
                            syllables$syllable_ids[preceding_cs[current_consonant]] <- current_nucleus

                            # Add it to the syllable text
                            syllables$syllable_text[current_nucleus] <- paste0(c(syllables$split_string[preceding_cs[current_consonant]],
                                                                                 syllables$syllable_text[current_nucleus]),
                                                                               collapse = "")

                            # Update the onset text
                            syllables$onset_text[current_nucleus] <- onset

                            # Update the onset position with the position of the just-added C
                            if (length(preceding_cs) > 0) syllables$onset_position[current_nucleus] <- preceding_cs[current_consonant]

                        } # If the profile is bad and the potential onset isn't exceptional, leave current C unparsed
                    } # If the resulting onset is banned, leave current C unparsed

                    # Update the counter to check the next C on the next loop
                    current_consonant <- current_consonant - 1
                }
            } # If the nucleus IS word-initial, do nothing

            # Update the counter to check the next nucleus on the next loop
            current_nucleus <- current_nucleus - 1
        }
    }
    return(syllables)
}

##########################
# Assign codas to nuclei #
##########################
assign_codas <- function(list) {
    # Add some elements to the list
    list$coda_text <- NULL
    list$coda_position <- NULL

    syllables <- list

    # Make sure the input is compatible w/ what the function needs
    if (class(list) == "character") {
        stop("The input to this function must be the output of assign_nuclei()")
    } else {
        # Only try to assign codas if there are unparsed Cs
        if ("C" %in% syllables$syllable_ids) {
            # Where are the unparsed Cs?
            unparsed_cs <- which(syllables$syllable_ids == "C")
            # For each unparsed C, assign it to the preceding syllable
            for (c in seq(length(unparsed_cs))) {
                current_c <- as.numeric(unparsed_cs[c])
                syllables$syllable_ids[current_c] <- syllables$syllable_ids[current_c - 1]
                current_syllable <- as.numeric(syllables$syllable_ids[current_c])

                # Add it to the syllable text
                syllables$syllable_text[current_syllable] <- paste0(c(syllables$syllable_text[current_syllable],
                                                                      syllables$split_string[current_c]),
                                                                    collapse = "")

                # Add it to the coda text
                syllables$coda_text[current_syllable] <- paste0(c(syllables$coda_text[current_syllable],
                                                                  syllables$split_string[current_c]),
                                                                collapse = "")

                # Record the position where the coda starts
                # Do this technically by first checking the preceding segment
                # If the preceding segment is a V, then this coda is the start of the coda
                # If the preceding segment is a C, then this coda is NOT the start of the coda
                if (syllables$cv_structure[current_c - 1] == "V") syllables$coda_position[current_syllable] <- current_c
            }
        }
    }
    # Return the list
    return(syllables)
}

###############################################
# Parse phonetic transcription into syllables #
###############################################
syllabify <- function(str,
                      diphthongs = en_diphthongs,
                      onsets = en_onsets,
                      verbose = FALSE) {

    # Assign nuclei to the string
    nuclei <- assign_nuclei(str)

    # Apply onsets to the nuclei
    onsets_and_nuclei <- assign_onsets(nuclei)

    # Parse the remaining consonants into codas
    syllables <- assign_codas(onsets_and_nuclei)

    if (verbose) {
        # Return the result of applying all the functions
        return(syllables)
    } else {
        # Make a nice visualization of the syllabified string
        syllabified_str <- paste0(syllables$syllable_text, collapse = ".")
        # And return that
        return(syllabified_str)
    }
}
