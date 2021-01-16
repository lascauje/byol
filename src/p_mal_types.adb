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

with Ada.Text_IO;
with P_Repl_Env;
with P_Regexp;

package body P_Mal_Types is
   function "<" (Left, Right : Symbol_Data) return Boolean is
   begin
      return Asu."<" (Left.Value, Right.Value);
   end "<";

   function "<" (Left, Right : Data) return Boolean is
   begin
      --  Hash_Maps Key is either Keyword or Str
      if Left in Keyword_Data and then Right in Keyword_Data
      then
         return Asu."<" (Keyword_Data (Left).Value,
                         Keyword_Data (Right).Value);
      elsif Left in Str_Data and then Right in Str_Data
      then
         return Asu."<" (Str_Data (Left).Value,
                         Str_Data (Right).Value);
      elsif Left in Str_Data then
         return True;
      else
         return False;
      end if;
   end "<";

   procedure Add (Self : in out Hash_Map_Of_Data; Cnt : Vectors.Vector) is
      Ind : Positive := 1;
   begin
      while Ind < Positive (Cnt.Length) loop
         --  Hash_Maps Key is either Keyword or Str
         if Cnt.Element (Ind) in P_Mal_Types.Keyword_Data then
            Self.Values.Insert
              (P_Mal_Types.Keyword_Data
                 (Cnt.Element (Ind)), Cnt.Element (Ind + 1));
         else
            Self.Values.Insert
              (P_Mal_Types.Str_Data
                 (Cnt.Element (Ind)), Cnt.Element (Ind + 1));
         end if;
         Ind := Ind + 2;
      end loop;
   end Add;

   function Escape_Char (Search_In : String) return String is
      First, Last : Positive;
      Found : Boolean;
      Buffer : Asu.Unbounded_String := Asu.To_Unbounded_String (Search_In);
      Current_First : Positive := Search_In'First;
   begin
      loop
         --  Replace \ by \\
         P_Regexp.Search_For_Pattern
           ("(\\)", Asu.Slice
              (Buffer, Current_First, Asu.Length (Buffer)),
            First, Last, Found);
         exit when not Found;
         Buffer := Asu.Replace_Slice ((Buffer), First, Last, "\\");
         --  Remove \ and add \\, so + 2
         Current_First := Last + 2;
      end loop;
      Current_First := Search_In'First;
      loop
         --  Replace " by \"
         P_Regexp.Search_For_Pattern
           ("(\"")", Asu.Slice
              (Buffer, Current_First, Asu.Length (Buffer)),
            First, Last, Found);
         exit when not Found;
         Buffer := Asu.Replace_Slice ((Buffer), First, Last, "\""");
         --  Remove " and add \", so + 2
         Current_First := Last + 2;
      end loop;
      return Asu.To_String (Buffer);
   end Escape_Char;

   function Evaluation_Char (Search_In : String) return String is
      First, Last : Positive;
      Found : Boolean;
      Buffer : Asu.Unbounded_String := Asu.To_Unbounded_String (Search_In);
      Current_First : Positive := Search_In'First;
      CR : constant Character := Character'Val (13);
      LF : constant Character := Character'Val (10);
      NL : constant String := CR & LF;
   begin
      loop
         --  Replace \\ by \
         P_Regexp.Search_For_Pattern
           ("(\\\\)", Asu.Slice
              (Buffer, Current_First, Asu.Length (Buffer)),
            First, Last, Found);
         exit when not Found;
         Buffer := Asu.Replace_Slice ((Buffer), First, Last, "\");
         --  No incr because remove one character
         --  Current_First := Last + 1;
      end loop;
      Current_First := Search_In'First;
      loop
         --  Replace \n by Ada new line
         P_Regexp.Search_For_Pattern
           ("(\\n)", Asu.Slice
              (Buffer, Current_First, Asu.Length (Buffer)),
            First, Last, Found);
         exit when not Found;
         Buffer := Asu.Replace_Slice ((Buffer), First, Last, NL);
         Current_First := Last + 1;
      end loop;
      Current_First := Search_In'First;
      loop
         --  Replace \" by "
         P_Regexp.Search_For_Pattern
           ("(\\"")", Asu.Slice
              (Buffer, Current_First, Asu.Length (Buffer)),
            First, Last, Found);
         exit when not Found;
         Buffer := Asu.Replace_Slice ((Buffer), First, Last, """");
         --  No incr because remove one character
         --  Current_First := Last + 1;
      end loop;
      return Asu.To_String (Buffer);
   end Evaluation_Char;

   --  TODO: this function could be moved on printer.adb file
   function Pretty_Print (Elem : Data; Esc_Char : Boolean;
                          Unwrap_Str : Boolean;
                          Eval_Char : Boolean)
                         return String
   is
      Type_Error : exception;
      Msg_Error : Asu.Unbounded_String;
      Buffer : Asu.Unbounded_String;
      Vct : Vectors.Vector;
   begin
      if Elem in List_Of_Data or else Elem in Vector_Of_Data then
         if Elem in List_Of_Data then
            Vct := List_Of_Data (Elem).Values;
            Buffer := Asu."&" (Buffer, "(");
         else
            Vct := Vector_Of_Data (Elem).Values;
            Buffer := Asu."&" (Buffer, "[");
         end if;
         for I in Vct.First_Index .. Vct.Last_Index loop
            if I > Vct.First_Index then
               Buffer := Asu."&" (Buffer, " ");
            end if;
            Buffer := Asu."&" (Buffer, Pretty_Print (Vct.Element (I),
                                                     Esc_Char,
                                                     Unwrap_Str,
                                                     Eval_Char));
         end loop;
         if Elem in List_Of_Data then
            Buffer := Asu."&" (Buffer, ")");
         else
            Buffer := Asu."&" (Buffer, "]");
         end if;
      elsif Elem in Hash_Map_Of_Data then
         Buffer := Asu."&" (Buffer, "{");
         for Csr in Hash_Map_Of_Data (Elem).Values.Iterate loop
            if P_Mal_Types.Maps.Element (Csr) /=
              Hash_Map_Of_Data (Elem).Values.First_Element
            then
               Buffer := Asu."&" (Buffer, " ");
            end if;
            Buffer := Asu."&" (Buffer, Pretty_Print
                                 (P_Mal_Types.Maps.Key (Csr),
                                  Esc_Char,
                                  Unwrap_Str,
                                  Eval_Char));
            Buffer := Asu."&" (Buffer, " ");
            Buffer := Asu."&" (Buffer, Pretty_Print
                                 (P_Mal_Types.Maps.Element (Csr),
                                  Esc_Char,
                                  Unwrap_Str,
                                  Eval_Char));
         end loop;
         Buffer := Asu."&" (Buffer, "}");
      elsif Elem in Str_Data then
         Buffer := Str_Data (Elem).Value;
         if Esc_Char then
            Buffer := Asu.To_Unbounded_String
              (Escape_Char (Asu.To_String (Buffer)));
         end if;
         if Unwrap_Str then
            --  Replace "foobar" by foobar
            --  1 2 "abc" "def" -> 1 2 abc def
            Buffer := Asu.Unbounded_Slice (Buffer, 2, Asu.Length (Buffer) - 1);
         end if;
         if Eval_Char then
            Buffer := Asu.To_Unbounded_String
              (Evaluation_Char (Asu.To_String (Buffer)));
         end if;
      elsif Elem in None_Data then
         Buffer := Asu.To_Unbounded_String ("nil");
      elsif Elem in Bool_Data then
         Buffer := (if Bool_Data (Elem).Value = True
                    then Asu.To_Unbounded_String ("true")
                    else Asu.To_Unbounded_String ("false"));
      elsif Elem in Int_Data then
         Buffer :=
           Asu.To_Unbounded_String (Integer'Image (Int_Data (Elem).Value));
         if Asu.Element (Buffer, 1) = ' ' then
            Buffer := Asu.Unbounded_Slice (Buffer, 2, Asu.Length (Buffer));
         end if;
      elsif Elem in Keyword_Data then
         Buffer := Asu."&" (":", Keyword_Data (Elem).Value);
      elsif Elem in Symbol_Data then
         Buffer := Symbol_Data (Elem).Value;
      elsif Elem in Empty_Data then
         Buffer := Asu.To_Unbounded_String ("");
      elsif Elem in P_Repl_Env.Fn_Def_Data then
         Buffer := Asu.To_Unbounded_String ("#<function>");
      elsif Elem in P_Repl_Env.Fn_Op_Data then
         Buffer := Asu.To_Unbounded_String ("#<function>");
      else
         Msg_Error := Asu.To_Unbounded_String ("type not found during print");
         Ada.Text_IO.Put_Line (Asu.To_String (Msg_Error));
         raise Type_Error with Asu.To_String (Msg_Error);
      end if;
      return Asu.To_String (Buffer);
   end Pretty_Print;
end P_Mal_Types;
