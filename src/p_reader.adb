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

with Ada.Text_IO; use Ada.Text_IO;
with P_Regexp;
with P_Mal_Types;
with Ada.Strings.Unbounded;

package body P_Reader is
   package Asu renames Ada.Strings.Unbounded;

   function Init (Tokens : Vectors.Vector; Position : Natural := 1)
                 return T_Reader is
      Self : T_Reader := (Tokens, Position);
   begin
      return Self;
   end Init;

   procedure Next (Self : in out T_Reader) is
   begin
      --  WARNING: Be ware when Peek function is used
      --  may the index out of the array
      Self.Position := Self.Position + 1;
   end Next;

   function Peek (Self : T_Reader) return String is
   begin
      return Self.Tokens.Element (Self.Position);
   end Peek;

   function Read_Atom (Reader : in out T_Reader) return P_Mal_Types.Data is
      Empty : P_Mal_Types.Empty_Data;
      Int_Pattern : constant String := "(-?[0-9]+$)";
      Token : Asu.Unbounded_String;
      First, Last : Positive;
      Found : Boolean;
      None :  P_Mal_Types.None_Data;
      Bool :  P_Mal_Types.Bool_Data;
      Int : P_Mal_Types.Int_Data;
      Str : P_Mal_Types.Str_Data;
      Keyword : P_Mal_Types.Keyword_Data;
      Symbol : P_Mal_Types.Symbol_Data;
      Parsing_Error : exception;
      Msg_Error : Asu.Unbounded_String;
   begin
      Token := Asu.To_Unbounded_String (Reader.Peek);
      P_Regexp.Search_For_Pattern (Int_Pattern,
                                   Asu.To_String (Token),
                                   First, Last, Found);
      if Found
        and then First = 1
        and then Last = Asu.Length (Token)
      then
         Int.Value := Integer'Value (Asu.To_String (Token));
         return Int;
      elsif Asu.Element (Token, 1) = '"' then
         if Asu.Element (Token, Asu.Length (Token)) = '"' then
            Str.Value := Token;
            return Str;
         else
            Msg_Error := Asu.To_Unbounded_String ("expected '""""', got EOF");
            Ada.Text_IO.Put_Line (Asu.To_String (Msg_Error));
            raise Parsing_Error with Asu.To_String (Msg_Error);
         end if;
      elsif Asu.Element (Token, 1) = ':' then
         Keyword.Value :=
           Asu.To_Unbounded_String (Asu.Slice (Token, 2, Asu.Length (Token)));
         return Keyword;
      elsif Asu.To_String (Token) = "nil" then
         return None;
      elsif Asu.To_String (Token) = "true" or else
        Asu.To_String (Token) = "false"
      then
         Bool.Value := (if Asu.To_String (Token) = "true" then True
                        else False);
         return Bool;
      else
         Symbol.Value := Token;
         return Symbol;
      end if;
   end Read_Atom;

   function Read_Form (Reader : in out T_Reader) return P_Mal_Types.Data is
      Token_Data : P_Mal_Types.Str_Data;
      Token : Asu.Unbounded_String;
      Empty : P_Mal_Types.Empty_Data;
      List : P_Mal_Types.List_Of_Data;
      Parsing_Error : exception;
      Msg_Error : Asu.Unbounded_String;
   begin
      Token := Asu.To_Unbounded_String (Reader.Peek);
      if Asu.Element (Token, 1) = ';' then
         Reader.Next;
         return Empty;
      elsif Asu.To_String (Token) = "'" then
         Reader.Next;
         Token_Data.Value := Token;
         List.Values.Append (P_Mal_Types.Symbol_Data'
                               (Value => Asu.To_Unbounded_String ("quote")));
         List.Values.Append (Read_Form (Reader));
         return List;
      elsif Asu.To_String (Token) = "`" then
         Reader.Next;
         Token_Data.Value := Token;
         List.Values.Append (P_Mal_Types.Symbol_Data'
                               (Value => Asu.To_Unbounded_String
                                  ("quasiquote")));
         List.Values.Append (Read_Form (Reader));
         return List;
      elsif Asu.To_String (Token) = "~" then
         Reader.Next;
         Token_Data.Value := Token;
         List.Values.Append (P_Mal_Types.Symbol_Data'
                               (Value => Asu.To_Unbounded_String
                                  ("unquote")));
         List.Values.Append (Read_Form (Reader));
         return List;
      elsif Asu.To_String (Token) = "~@" then
         Reader.Next;
         Token_Data.Value := Token;
         List.Values.Append (P_Mal_Types.Symbol_Data'
                               (Value => Asu.To_Unbounded_String
                                  ("splice-unquote")));
         List.Values.Append (Read_Form (Reader));
         return List;
      elsif Asu.To_String (Token) = "^" then
         Reader.Next;
         Token_Data.Value := Token;
         List.Values.Append (P_Mal_Types.Symbol_Data'
                               (Value => Asu.To_Unbounded_String
                                  ("with-meta")));
         List.Values.Append (Read_Form (Reader));
         Reader.Next;
         List.Values.Insert (List.Values.Last_Index, Read_Form (Reader));
         return List;
      elsif Asu.To_String (Token) = "@" then
         Reader.Next;
         Token_Data.Value := Token;
         List.Values.Append (P_Mal_Types.Symbol_Data'
                               (Value => Asu.To_Unbounded_String
                                  ("deref")));
         List.Values.Append (Read_Form (Reader));
         return List;
         --  List
      elsif Asu.To_String (Token) = ")" then
         Msg_Error := Asu.To_Unbounded_String ("unexpected ')'");
         Ada.Text_IO.Put_Line (Asu.To_String (Msg_Error));
         raise Parsing_Error with Asu.To_String (Msg_Error);
      elsif Asu.To_String (Token) = "(" then
         return Read_List (Reader);
         --  Vector
      elsif Asu.To_String (Token) = "]" then
         Msg_Error := Asu.To_Unbounded_String ("unexpected ']'");
         Ada.Text_IO.Put_Line (Asu.To_String (Msg_Error));
         raise Parsing_Error with Asu.To_String (Msg_Error);
      elsif Asu.To_String (Token) = "[" then
         return Read_Vector (Reader);
         --  Hash-Map
      elsif Asu.To_String (Token) = "}" then
         Msg_Error := Asu.To_Unbounded_String ("unexpected '}'");
         Ada.Text_IO.Put_Line (Asu.To_String (Msg_Error));
         raise Parsing_Error with Asu.To_String (Msg_Error);
      elsif Asu.To_String (Token) = "{" then
         return Read_Hash_Map (Reader);
      else
         return Read_Atom (Reader);
      end if;
   end Read_Form;

   function Read_Hash_Map (Reader : in out T_Reader)
                          return P_Mal_Types.Data is
      Hash_Map : P_Mal_Types.Hash_Map_Of_Data;
      Container : P_Mal_Types.Vectors.Vector;
   begin
      Container := Read_Sequence (Reader, "{", "}");
      Hash_Map.Add (Container);
      return Hash_Map;
   end Read_Hash_Map;

   function Read_List (Reader : in out T_Reader)
                      return P_Mal_Types.Data is
      List : P_Mal_Types.List_Of_Data;
   begin
      List.Values := Read_Sequence (Reader, "(", ")");
      return List;
   end Read_List;

   function Read_Sequence (Reader : in out T_Reader;
                           Del_Start : String; Del_End : String)
                          return P_Mal_Types.Vectors.Vector is
      Container : P_Mal_Types.Vectors.Vector;
      Token : Asu.Unbounded_String;
      Parsing_Error : exception;
      Msg_Error : Asu.Unbounded_String;
   begin
      Token := Asu.To_Unbounded_String (Reader.Peek);
      Reader.Next;
      if Asu.To_String (Token) /= Del_Start then
         Msg_Error := Asu.To_Unbounded_String ("expected " & Del_Start);
         Ada.Text_IO.Put_Line (Asu.To_String (Msg_Error));
         raise Parsing_Error with Asu.To_String (Msg_Error);
      end if;
      Token := Asu.To_Unbounded_String (Reader.Peek);
      while Asu.To_String (Token) /= Del_End loop
         Container.Append (Read_Form (Reader));
         Reader.Next;
         Token := Asu.To_Unbounded_String (Reader.Peek);
      end loop;
      return Container;
   exception
      when others =>
         Msg_Error := Asu.To_Unbounded_String
           ("expected '" & Del_End & "', got EOF");
         Ada.Text_IO.Put_Line (Asu.To_String (Msg_Error));
         raise Parsing_Error with Asu.To_String (Msg_Error);
   end Read_Sequence;

   function Read_Str (Search_In : String)
                     return P_Mal_Types.Data is
      Reader : T_Reader;
   begin
      Reader := Init (Tokenizer (Search_In));
      return Read_Form (Reader);
   end Read_Str;

   function Read_Vector (Reader : in out T_Reader)
                        return P_Mal_Types.Data is
      Vector : P_Mal_Types.Vector_Of_Data;
   begin
      Vector.Values := Read_Sequence (Reader, "[", "]");
      return Vector;
   end Read_Vector;

   function Tokenizer (Search_In : String) return Vectors.Vector is
      Word_Pattern : constant String := "((~@|[\[\]{}()'`~^@]|" &
        """(?:\\.|[^\\""])*""|;.*|[^\s\[\]{}('""`,;)]+))";
      Currrent_First : Positive := Search_In'First;
      First, Last : Positive;
      Found : Boolean;
      Tokens : Vectors.Vector;
   begin
      loop
         P_Regexp.Search_For_Pattern (Word_Pattern,
                                      Search_In
                                        (Currrent_First .. Search_In'Last),
                                      First, Last, Found);
         exit when not Found;
         Tokens.Append (Search_In (First .. Last));
         Currrent_First := Last + 1;
      end loop;
      return Tokens;
   end Tokenizer;
end P_Reader;
