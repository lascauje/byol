# This file is part of Lispy.

# Lispy is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Lispy is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Lispy.  If not, see <https://www.gnu.org/licenses/>.

cd "$(dirname "$0")"

# -gnatVa: All validity checks
# -gnatwe: Warnings as Errors
# -gnatyy: Set all standard style check options(gnaty3aAbcefhiklmnprst),
# that is all checking options enabled with the exception of
# -gnatyB, -gnatyd, -gnatyI, -gnatyLnnn, -gnatyo, -gnatyO,
# -gnatyS, -gnatyu, -gnatyx

gnatmake lispy.adb -g -gnatVa -gnaty3aAbcefhiklmnprst -gnatyB -gnatyd -gnatyI -gnatyL7 -gnatyo -gnatyO -gnatyS -gnatyu -gnatyx -gnatwe
