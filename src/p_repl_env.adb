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

with P_Reader;
with Ada.Text_IO;

package body P_Repl_Env is
   function "<" (Left, Right : Asu.Unbounded_String) return Boolean is
   begin
      return Asu."<" (Left, Right);
   end "<";

   function Count (Args : P_Mal_Types.Data)
                  return P_Mal_Types.Data is
      Int : P_Mal_Types.Int_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
      Count_Error : exception;
   begin
      if Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.None_Data
      then
         Int.Value := 0;
      elsif Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.List_Of_Data
      then
         Int.Value := Integer (P_Mal_Types.List_Of_Data
                                 (Lst_Args.Values.Element
                                    (Lst_Args.Values.First_Index)).
                                 Values.Length);
      elsif Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.Vector_Of_Data
      then
         Int.Value := Integer (P_Mal_Types.Vector_Of_Data
                                 (Lst_Args.Values.Element
                                    (Lst_Args.Values.First_Index)).
                                 Values.Length);
      else
         raise Count_Error with "count called with invalid args";
      end if;
      return Int;
   end Count;

   function Div (Args : P_Mal_Types.Data)
                return P_Mal_Types.Data is
      Int : P_Mal_Types.Int_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
   begin
      Int.Value := P_Mal_Types.Int_Data
        (Lst_Args.Values.Element
           (Lst_Args.Values.First_Index)).Value /
        P_Mal_Types.Int_Data
          (Lst_Args.Values.Element
             (Lst_Args.Values.First_Index + 1)).Value;
      return Int;
   end Div;

   function Do_Str (Args : P_Mal_Types.Data)
                   return P_Mal_Types.Data is
      Buffer : Asu.Unbounded_String;
      Str : P_Mal_Types.Str_Data;
   begin
      for Elem of P_Mal_Types.List_Of_Data (Args).Values loop
         Buffer := Asu."&" (Buffer, Asu.To_Unbounded_String
                              (P_Mal_Types.Pretty_Print
                                 (Elem, False, True, False)));
      end loop;
      --  Add """" and """"
      Buffer := Asu."&" ("""", Buffer);
      Buffer := Asu."&" (Buffer, """");
      Str.Value := Buffer;
      return Str;
   end Do_Str;

   function Empty_Q (Args : P_Mal_Types.Data)
   return P_Mal_Types.Data is
      Bool : P_Mal_Types.Bool_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
      Empty_Error : exception;
   begin
      if Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.List_Of_Data
      then
         Bool.Value := P_Mal_Types.List_Of_Data
           (Lst_Args.Values.Element
              (Lst_Args.Values.First_Index)).Values.Is_Empty;
      elsif Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.Vector_Of_Data
      then
         Bool.Value := P_Mal_Types.Vector_Of_Data
           (Lst_Args.Values.Element
              (Lst_Args.Values.First_Index)).Values.Is_Empty;
      else
         raise Empty_Error with "empty called with invalid args";
      end if;
      return Bool;
   end Empty_Q;

   --  TODO: refactor with template method?
   function Equal (Args : P_Mal_Types.Data)
                  return P_Mal_Types.Data is
      Bool : P_Mal_Types.Bool_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
      Lst_Args_Tmp : P_Mal_Types.List_Of_Data;
      Vct_Op_L : P_Mal_Types.Vectors.Vector;
      Vct_Op_R : P_Mal_Types.Vectors.Vector;
   begin
      if Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.Int_Data and then
        Lst_Args.Values.Element (Lst_Args.Values.First_Index + 1) in
        P_Mal_Types.Int_Data
      then
         Bool.Value := P_Mal_Types.Int_Data
           (Lst_Args.Values.Element
              (Lst_Args.Values.First_Index)).Value =
           P_Mal_Types.Int_Data
             (Lst_Args.Values.Element
                (Lst_Args.Values.First_Index + 1)).Value;
      elsif Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.Bool_Data and then
        Lst_Args.Values.Element (Lst_Args.Values.First_Index + 1) in
        P_Mal_Types.Bool_Data
      then
         Bool.Value := P_Mal_Types.Bool_Data
           (Lst_Args.Values.Element
              (Lst_Args.Values.First_Index)).Value =
           P_Mal_Types.Bool_Data
             (Lst_Args.Values.Element
                (Lst_Args.Values.First_Index + 1)).Value;
      elsif Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.Str_Data and then
        Lst_Args.Values.Element (Lst_Args.Values.First_Index + 1) in
        P_Mal_Types.Str_Data
      then
         Bool.Value := Asu."="
           (P_Mal_Types.Str_Data
              (Lst_Args.Values.Element
                 (Lst_Args.Values.First_Index)).Value,
            P_Mal_Types.Str_Data
              (Lst_Args.Values.Element
                 (Lst_Args.Values.First_Index + 1)).Value);
      elsif Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.Keyword_Data and then
        Lst_Args.Values.Element (Lst_Args.Values.First_Index + 1) in
        P_Mal_Types.Keyword_Data
      then
         Bool.Value := Asu."="
           (P_Mal_Types.Keyword_Data
              (Lst_Args.Values.Element
                 (Lst_Args.Values.First_Index)).Value,
            P_Mal_Types.Keyword_Data
              (Lst_Args.Values.Element
                 (Lst_Args.Values.First_Index + 1)).Value);
      elsif Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.None_Data and then
        Lst_Args.Values.Element (Lst_Args.Values.First_Index + 1) in
        P_Mal_Types.None_Data
      then
         Bool.Value := True;
      elsif Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.List_Of_Data | P_Mal_Types.Vector_Of_Data
        and then
        Lst_Args.Values.Element (Lst_Args.Values.First_Index + 1) in
        P_Mal_Types.List_Of_Data | P_Mal_Types.Vector_Of_Data
      then
         --  Get the vector for the left operand
         if Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
           P_Mal_Types.List_Of_Data
         then
            Vct_Op_L := P_Mal_Types.List_Of_Data
              (Lst_Args.Values.Element
                 (Lst_Args.Values.First_Index)).Values;
         else
            Vct_Op_L := P_Mal_Types.Vector_Of_Data
              (Lst_Args.Values.Element
                 (Lst_Args.Values.First_Index)).Values;
         end if;
         --  Get the vector for the right operand
         if Lst_Args.Values.Element (Lst_Args.Values.First_Index + 1) in
           P_Mal_Types.List_Of_Data
         then
            Vct_Op_R := P_Mal_Types.List_Of_Data
              (Lst_Args.Values.Element
                 (Lst_Args.Values.First_Index + 1)).Values;
         else
            Vct_Op_R := P_Mal_Types.Vector_Of_Data
              (Lst_Args.Values.Element
                 (Lst_Args.Values.First_Index + 1)).Values;
         end if;
         if Integer (Vct_Op_L.Length) /=
           Integer (Vct_Op_R.Length)
         then
            Bool.Value := False;
         else
            Bool.Value := True;
            for I in Vct_Op_L.First_Index ..
              Vct_Op_L.Last_Index loop
               --  In order to compare all elements,
               --  we need to recall Equal function on each,
               --  so we create a temporary list
               Lst_Args_Tmp.Values.Append (Vct_Op_L.Element (I));
               Lst_Args_Tmp.Values.Append (Vct_Op_R.Element (I));
               if not P_Mal_Types.Bool_Data (Equal (Lst_Args_Tmp)).Value then
                  Bool.Value := False;
                  exit;
               end if;
            end loop;
         end if;
      else
         Bool.Value := False;
      end if;
      return Bool;
   end Equal;

   function Inf (Args : P_Mal_Types.Data)
                return P_Mal_Types.Data is
      Bool : P_Mal_Types.Bool_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
   begin
      Bool.Value := P_Mal_Types.Int_Data
        (Lst_Args.Values.Element
           (Lst_Args.Values.First_Index)).Value <
        P_Mal_Types.Int_Data
          (Lst_Args.Values.Element
             (Lst_Args.Values.First_Index + 1)).Value;
      return Bool;
   end Inf;

   function Inf_Equal (Args : P_Mal_Types.Data)
                      return P_Mal_Types.Data is
      Bool : P_Mal_Types.Bool_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
   begin
      Bool.Value := P_Mal_Types.Int_Data
        (Lst_Args.Values.Element
           (Lst_Args.Values.First_Index)).Value <=
        P_Mal_Types.Int_Data
          (Lst_Args.Values.Element
             (Lst_Args.Values.First_Index + 1)).Value;
      return Bool;
   end Inf_Equal;

   function Init_Core_Ns return Core_Ns.Map is
      New_Core_Ns : Core_Ns.Map;
   begin
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("read-string"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Read_String'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("prn"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Prn'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("pr-str"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Pr_Str'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("str"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Do_Str'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("println"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Println'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("count"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Count'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("empty?"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Empty_Q'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("list"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.List'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("list?"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.List_Q'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String (">"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Sup'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String (">="),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Sup_Equal'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("<"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Inf'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("<="),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Inf_Equal'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("="),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Equal'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("+"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Plus'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("-"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Min'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("*"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Mult'Access));
      New_Core_Ns.Insert (Asu.To_Unbounded_String ("/"),
                          P_Repl_Env.Fn_Op_Data'
                            (Value => P_Repl_Env.Div'Access));
      return New_Core_Ns;
   end Init_Core_Ns;

   function List (Args : P_Mal_Types.Data)
                 return P_Mal_Types.Data is
   begin
      return P_Mal_Types.List_Of_Data (Args);
   end List;

   function List_Q (Args : P_Mal_Types.Data)
                   return P_Mal_Types.Data is
      Bool : P_Mal_Types.Bool_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
   begin
      Bool.Value := Lst_Args.Values.Element (Lst_Args.Values.First_Index) in
        P_Mal_Types.List_Of_Data;
      return Bool;
   end List_Q;

   function Min (Args : P_Mal_Types.Data)
                return P_Mal_Types.Data is
      Int : P_Mal_Types.Int_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
   begin
      Int.Value := P_Mal_Types.Int_Data
        (Lst_Args.Values.Element
           (Lst_Args.Values.First_Index)).Value -
        P_Mal_Types.Int_Data
          (Lst_Args.Values.Element
             (Lst_Args.Values.First_Index + 1)).Value;
      return Int;
   end Min;

   function Mult (Args : P_Mal_Types.Data)
                 return P_Mal_Types.Data is
      Int : P_Mal_Types.Int_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
   begin
      Int.Value := P_Mal_Types.Int_Data
        (Lst_Args.Values.Element
           (Lst_Args.Values.First_Index)).Value *
        P_Mal_Types.Int_Data
          (Lst_Args.Values.Element
             (Lst_Args.Values.First_Index + 1)).Value;
      return Int;
   end Mult;

   function Plus (Args : P_Mal_Types.Data)
                 return P_Mal_Types.Data is
      Int : P_Mal_Types.Int_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
   begin
      Int.Value := P_Mal_Types.Int_Data
        (Lst_Args.Values.Element
           (Lst_Args.Values.First_Index)).Value +
        P_Mal_Types.Int_Data
          (Lst_Args.Values.Element
             (Lst_Args.Values.First_Index + 1)).Value;
      return Int;
   end Plus;

   function Pr_Str (Args : P_Mal_Types.Data)
                   return P_Mal_Types.Data is
      Buffer : Asu.Unbounded_String;
      Str : P_Mal_Types.Str_Data;
   begin
      Buffer := Asu.To_Unbounded_String
        (P_Mal_Types.Pretty_Print (Args, True, False, False));
      --  Add """ and """ and remove "(" and ")": (\"foo\") -> "\"foo\""
      Asu.Replace_Element (Buffer, 1, '"');
      Asu.Replace_Element (Buffer, Asu.Length (Buffer), '"');
      Str.Value := Buffer;
      return Str;
   end Pr_Str;

   function Println (Args : P_Mal_Types.Data)
                    return P_Mal_Types.Data is
      Buffer : Asu.Unbounded_String;
      None : P_Mal_Types.None_Data;
   begin
      Buffer := Asu.To_Unbounded_String
        (P_Mal_Types.Pretty_Print (Args, False, True, True));
      --  Remove "(" and ")"
      Buffer := Asu.Unbounded_Slice (Buffer, 2, Asu.Length (Buffer) - 1);
      Ada.Text_IO.Put_Line (Asu.To_String (Buffer));
      return None;
   end Println;

   function Prn (Args : P_Mal_Types.Data)
                return P_Mal_Types.Data is
      Buffer : Asu.Unbounded_String;
      None : P_Mal_Types.None_Data;
   begin
      Buffer := Asu.To_Unbounded_String
        (P_Mal_Types.Pretty_Print (Args, False, False, False));
      --  Remove "(" and ")"
      Buffer := Asu.Unbounded_Slice (Buffer, 2, Asu.Length (Buffer) - 1);
      Ada.Text_IO.Put_Line (Asu.To_String (Buffer));
      return None;
   end Prn;

   function Read_String (Args : P_Mal_Types.Data)
                        return P_Mal_Types.Data is
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
      Buffer : Asu.Unbounded_String;
   begin
      Buffer := P_Mal_Types.Str_Data
        (Lst_Args.Values.Element
           (Lst_Args.Values.First_Index)).Value;
      --  (read-string "(+ 2 3)")
      --  In Ada code is a string because there is quotes on quotes ""(+ 2 3)""
      --  So before call P_Reader.Read_Str we remove quotes
      --  In order to have "(+ 2 3)"
      Buffer := Asu.Unbounded_Slice (Buffer, 2, Asu.Length (Buffer) - 1);
      return P_Reader.Read_Str (Asu.To_String (Buffer));
   end Read_String;

   function Sup (Args : P_Mal_Types.Data)
                return P_Mal_Types.Data is
      Bool : P_Mal_Types.Bool_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
   begin
      Bool.Value := P_Mal_Types.Int_Data
        (Lst_Args.Values.Element
           (Lst_Args.Values.First_Index)).Value >
        P_Mal_Types.Int_Data
          (Lst_Args.Values.Element
             (Lst_Args.Values.First_Index + 1)).Value;
      return Bool;
   end Sup;

   function Sup_Equal (Args : P_Mal_Types.Data)
                      return P_Mal_Types.Data is
      Bool : P_Mal_Types.Bool_Data;
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
   begin
      Bool.Value := P_Mal_Types.Int_Data
        (Lst_Args.Values.Element
           (Lst_Args.Values.First_Index)).Value >=
        P_Mal_Types.Int_Data
          (Lst_Args.Values.Element
             (Lst_Args.Values.First_Index + 1)).Value;
      return Bool;
   end Sup_Equal;

end P_Repl_Env;
