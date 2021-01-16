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

with Ada.Containers.Indefinite_Vectors;
with P_Mal_Types;

package P_Reader is
   --  tagged because I want to call object.method like
   type T_Reader is tagged private;
   package Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   function Init (Tokens : Vectors.Vector; Position : Natural := 1)
                 return T_Reader;
   function Peek (Self : T_Reader) return String;
   procedure Next (Self : in out T_Reader);
   function Tokenizer (Search_In : String) return Vectors.Vector;
   function Read_Str (Search_In : String) return P_Mal_Types.Data;
   function Read_Form (Reader : in out T_Reader) return P_Mal_Types.Data;
   function Read_List (Reader : in out T_Reader) return P_Mal_Types.Data;
   function Read_Vector (Reader : in out T_Reader) return P_Mal_Types.Data;
   function Read_Hash_Map (Reader : in out T_Reader) return P_Mal_Types.Data;
   function Read_Atom (Reader : in out T_Reader) return P_Mal_Types.Data;
   function Read_Sequence (Reader : in out T_Reader;
                           Del_Start : String; Del_End : String)
                          return P_Mal_Types.Vectors.Vector;
private
   type T_Reader is tagged record
      Tokens : Vectors.Vector;
      Position : Natural;
   end record;
end P_Reader;
