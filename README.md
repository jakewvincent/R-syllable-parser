*This README is under construction*

# Syllable parser (R implementation)

Newly updated syllable parser. See the old scripts [here](https://jakewvincent.github.com/R-syllable-parser_old).

This set of scripts defines several functions (the foremost of which is `syllabify()`) that together can syllabify a phonetic transcription using principles taught to students in Phonology 1.

The following functionality has not yet been incorporated into the new version.
> Some of the provided functions (namely, `transcribe()`) interface with a local copy of the Carnegie Mellon University (CMU) Pronouncing Dictionary, which provides phonetic transcriptions (using ARPABET sequences) for over 134,000 words in the English lexicon. Scripts are also provided that check the dictionary for updates, download the most recent version from the dictionary's website, and format it, including converting ARPABET codes to International Phonetic Alphabet (IPA) characters.

The parser basically uses a strategy taught to undergraduate students of Phonology when they are learning how to determine the syllabification of a word:

1. **Identify syllable nuclei** by looking for the vowels. If there are two adjacent vowels, check if those vowels together are a known diphthong in the language. If they are, consider them part of the same syllable nucleus. Standalone vowels each count as the nucleus of their own syllable.
2. **Identify syllable onsets** (this occurs before step 3 in accordance with the Onset Principle). Automatically parse consonants preceding the first vowel into the first syllable. Parse a consonant immediately preceding any of the other vowels into the syllable of that vowel. If any remaining unparsed consonants are not after the last vowel in the word and have a consonant following them, check if that consonant and the following are a legal onset in the language (defined as a consonant cluster that can occur at the beginning of a word, as long as that word is not a borrowing). If they're a legal onset, parse that consonant into the syllable of the consonant following it. If they're not a legal onset, leave that consonant unparsed.
3. **Identify syllable codas** by parsing any remaining unparsed consonants into the syllables preceding them.

## Getting started

### Prerequisites
1. [R programming language environment](https://www.r-project.org/about.html)
2. The following R packages:
   * [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
   * [magrittr](https://cran.r-project.org/web/packages/magrittr/index.html)
   * [readr](https://cran.r-project.org/web/packages/readr/index.html)
   * [tidyr](https://cran.r-project.org/web/packages/tidyr/index.html)
   * [stringr](https://cran.r-project.org/web/packages/stringr/index.html)
3. Optional packages:
   * [RCurl](https://cran.r-project.org/web/packages/RCurl/index.html) (Required for checking dictionary version and downloading updates)
   * [pbapply](https://cran.rstudio.com/web/packages/pbapply/index.html) (Required by dictionary formatter)

### Installing
#### Using Git
1. Clone this repository into a directory of your choosing: `git clone https://github.com/jakewvincent/R-syllable-parser.git`
2. Open an R terminal and set your working directory to the directory where you cloned this repository.
3. Source `master.R` by running `source(file = "master.R")` in your R terminal.

#### By downloading .zip file
1. [Download this repository as a zip file](https://github.com/jakewvincent/R-syllable-parser/archive/master.zip) and unzip it into a directory of your choosing.
2. Open an R terminal and set your working directory to the directory where you unzipped the zipped repository file.
3. Source `master.R` by running `source(file = "master.R")` in your R terminal.

## Using the parser (section under construction)
After sourcing `0_master.R` as above, all of the functions defined by these scripts are available for use. Some of these functions (namely, `cvify()` and `sonority()`) are mainly used internally. Here is a description of each function:

### `syllabify()`
### `cvify()`
### `sonority()`

## To do
* [ ] Re-incorporate CMU pronouncing dictionary
    * [ ] `random_word()` function
    * [ ] `transcribe()` function
* [ ] Make outputs when `verbose = TRUE` consistent and predictable
* [ ] Recognize syllabic consonants (e.g. words that end in [ɪzm] should have the [m] assigned to a nucleus)

