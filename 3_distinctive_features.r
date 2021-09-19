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

# Read in distinctive feature chart
features <- read.csv(file = "distinctive_features.csv")

# Assign the data frame an additional class in Windows
if (this_os == "windows") {
    class(features) <- c("unicode_df", "data.frame")
}
