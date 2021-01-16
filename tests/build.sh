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

env STEP=step0_repl ./runtest.py step0_repl.mal -- ../src/lispy --pretty_print_echo
env STEP=step1_read_print ./runtest.py step1_read_print.mal -- ../src/lispy --pretty_print
env STEP=step2_eval ./runtest.py step2_eval.mal -- ../src/lispy
env STEP=step3_env ./runtest.py step3_env.mal -- ../src/lispy
env STEP=step4_if_fn_do ./runtest.py step4_if_fn_do.mal -- ../src/lispy
env STEP=step6_file ./runtest.py step6_file.mal -- ../src/lispy
