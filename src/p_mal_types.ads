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

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

package P_Mal_Types is
   package Asu renames Ada.Strings.Unbounded;
   --  Tagged because inheritance, and null because empty record
   type Empty_Data is tagged null record;
   --  Data is an Empty_Data or all derivated class from Empty_Data
   subtype Data is Empty_Data'Class;
   function "<" (Left, Right : Data) return Boolean;

   type Atomic is new Empty_Data with null record;

   type None_Data is new Atomic with null record;

   type Str_Data is new Atomic with record
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Bool_Data is new Atomic with record
      Value : Boolean;
   end record;

   type Keyword_Data is new Atomic with record
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Symbol_Data is new Atomic with record
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   function "<" (Left, Right : Symbol_Data) return Boolean;

   type Int_Data is new Atomic with record
      Value : Integer;
   end record;

   package Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => Data);
   type List_Of_Data is new Empty_Data with record
      Values : Vectors.Vector;
   end record;

   type Vector_Of_Data is new Empty_Data with record
      Values : Vectors.Vector;
   end record;

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Data, Data);
   type Hash_Map_Of_Data is new Empty_Data with record
      Values : Maps.Map;
   end record;
   procedure Add (Self : in out Hash_Map_Of_Data; Cnt : Vectors.Vector);

   function Pretty_Print (Elem : Data; Esc_Char : Boolean;
                          Unwrap_Str : Boolean;
                          Eval_Char : Boolean)
                         return String;
   function Escape_Char (Search_In : String) return String;
   function Evaluation_Char (Search_In : String) return String;

end P_Mal_Types;
