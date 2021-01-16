--  This file is part of Lispy.

--  Lispy is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

--  Lispy is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  You should have received a copy of the GNU General Public License
--  along with Lispy.  If not, see <https://www.gnu.org/licenses/>.

with GNAT.Regpat;

package body P_Regexp is
   package Pat renames GNAT.Regpat;
   procedure Search_For_Pattern (Word_Pattern : String;
                                 Search_In : String;
                                 First, Last : out Positive;
                                 Found : out Boolean) is
      Result : Pat.Match_Array (0 .. 1);
      Compiled_Expression : Pat.Pattern_Matcher := Pat.Compile (Word_Pattern);
   begin
      Pat.Match (Compiled_Expression, Search_In, Result);
      Found := not Pat."=" (Result (1), Pat.No_Match);
      if Found then
         First := Result (1).First;
         Last := Result (1).Last;
      end if;
   end Search_For_Pattern;
end P_Regexp;
