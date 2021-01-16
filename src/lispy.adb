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
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with P_Mal_Types;
with P_Env;
with P_Repl_Env;
with P_Reader;

procedure Lispy is
   --  Global variable in order to call Do_Eval with only one args
   G_Env : P_Env.Env := P_Env.Init;

   package Asu renames Ada.Strings.Unbounded;

   function Eval (Ast : P_Mal_Types.Data;
                  Env : in out P_Env.Env) return P_Mal_Types.Data;

   procedure Init_Env (Env : in out P_Env.Env);

   function Print (Exp : P_Mal_Types.Data) return String;

   function Read (Str : String) return P_Mal_Types.Data;

   function Rep (Str : String; Env : in out P_Env.Env) return String;

   procedure Rep_No_Return (Str : String; Env : in out P_Env.Env);

   function Do_Eval (Args : P_Mal_Types.Data)
                      return P_Mal_Types.Data;

   function Do_Eval (Args : P_Mal_Types.Data)
                      return P_Mal_Types.Data is
      Lst_Args : P_Mal_Types.List_Of_Data := P_Mal_Types.List_Of_Data (Args);
   begin
      return Eval (Lst_Args.Values.Element
                     (Lst_Args.Values.First_Index), G_Env);
   end Do_Eval;

   function Eval (Ast : P_Mal_Types.Data;
                  Env : in out P_Env.Env) return P_Mal_Types.Data is
      function Eval_Ast (Ast : P_Mal_Types.Data;
                         Env : in out P_Env.Env) return P_Mal_Types.Data;
      function Eval_Ast (Ast : P_Mal_Types.Data;
                         Env : in out P_Env.Env) return P_Mal_Types.Data is
         Fn_Op : P_Repl_Env.Fn_Op_Data;
         Lst : P_Mal_Types.List_Of_Data;
         Vct : P_Mal_Types.Vector_Of_Data;
         Hsm : P_Mal_Types.Hash_Map_Of_Data;
         Cnt : P_Mal_Types.Vectors.Vector;
         Ast_Cnt : P_Mal_Types.Vectors.Vector;
      begin
         if Ast in P_Mal_Types.Symbol_Data then
            return Env.Get (P_Mal_Types.Symbol_Data (Ast));
         elsif Ast in P_Mal_Types.List_Of_Data or else
           Ast in P_Mal_Types.Vector_Of_Data
         then
            Ast_Cnt := (if Ast in P_Mal_Types.List_Of_Data
                        then P_Mal_Types.List_Of_Data (Ast).Values
                        else P_Mal_Types.Vector_Of_Data (Ast).Values);
            for I in Ast_Cnt.First_Index .. Ast_Cnt.Last_Index loop
               Cnt.Append (Eval (Ast_Cnt (I), Env));
            end loop;
            if Ast in P_Mal_Types.List_Of_Data then
               Lst.Values := Cnt;
               return Lst;
            else
               Vct.Values := Cnt;
               return Vct;
            end if;
         elsif Ast in P_Mal_Types.Hash_Map_Of_Data then
            for Csr in P_Mal_Types.Hash_Map_Of_Data (Ast).Values.Iterate loop
               Cnt.Append (Eval (P_Mal_Types.Maps.Key (Csr), Env));
               Cnt.Append (Eval (P_Mal_Types.Maps.Element (Csr), Env));
            end loop;
            Hsm.Add (Cnt);
            return Hsm;
         else
            return Ast;             -- Primitive value
         end if;
      end Eval_Ast;
      Fn_Op : P_Repl_Env.Fn_Op_Data;
      Fn_Def : P_Repl_Env.Fn_Def_Data;
      Sbl : P_Mal_Types.Symbol_Data;
      Lst : P_Mal_Types.List_Of_Data;
   begin
      --  Ast is not a List
      if Ast not in P_Mal_Types.List_Of_Data then
         return Eval_Ast (Ast, Env);
      end if;

      if P_Mal_Types.List_Of_Data (Ast).Values.Is_Empty then
         return Ast;
      end if;

      --  Ast is a list
      --  TODO: call _list function (List in Ada) instead
      Lst := P_Mal_Types.List_Of_Data (Ast);
      if Lst.Values.Element (Lst.Values.First_Index) in P_Mal_Types.Symbol_Data
      then
         Sbl := P_Mal_Types.Symbol_Data
           (Lst.Values.Element (Lst.Values.First_Index));
         if Asu.To_String (Sbl.Value) = "def!" then
            return Env.Set_Ret
              (P_Mal_Types.Symbol_Data
                 (Lst.Values.Element (Lst.Values.First_Index + 1)),
               Eval (Lst.Values.Element (Lst.Values.First_Index + 2), Env));
         elsif Asu.To_String (Sbl.Value) = "let*" then
            declare
               Ind : Positive := 1;
               Let_Env : P_Env.Env;
               Decl : P_Mal_Types.Vectors.Vector;
            begin
               if Lst.Values.Element (Lst.Values.First_Index + 1) in
                 P_Mal_Types.Vector_Of_Data
               then
                  Decl := P_Mal_Types.Vector_Of_Data
                    (Lst.Values.Element (Lst.Values.First_Index + 1)).Values;
               else
                  Decl := P_Mal_Types.List_Of_Data
                    (Lst.Values.Element (Lst.Values.First_Index + 1)).Values;
               end if;
               Let_Env := P_Env.Init (Env);
               while Ind < Positive (Decl.Length) loop
                  Let_Env.Set
                    (P_Mal_Types.Symbol_Data (Decl.Element (Ind)),
                     Eval (Decl.Element (Ind + 1), Let_Env));
                  Ind := Ind + 2;
               end loop;
               return Eval
                 (Lst.Values.Element (Lst.Values.First_Index + 2), Let_Env);
            end;
         elsif Asu.To_String (Sbl.Value) = "do" then
            declare
               Lst_Do : P_Mal_Types.List_Of_Data;
            begin
               Lst_Do.Values := P_Mal_Types.List_Of_Data (Ast).Values.Copy;
               Lst_Do.Values.Delete_First;
               return P_Mal_Types.List_Of_Data
                 (Eval_Ast (Lst_Do, Env)).Values.Last_Element;
            end;
         elsif Asu.To_String (Sbl.Value) = "if" then
            declare
               --  Hack: true by default because
               --  (if <int|""|list|...> <val1> <val2>) is always <val1>
               Cond : Boolean := True;
               None_Found : Boolean := False;
               --  TODO: use to return a None type,
               --  because I don't know how to return None without variable
               None : P_Mal_Types.None_Data;
            begin
               --  HACK: ugly because three evaluation
               --  (we need to know the type...)
               if Eval (Lst.Values.Element (Lst.Values.First_Index + 1), Env)
                 in P_Mal_Types.Bool_Data
               then
                  --  (if (> 5 3) "yes" "no")
                  Cond := P_Mal_Types.Bool_Data
                    (Eval (Lst.Values.Element
                             (Lst.Values.First_Index + 1), Env)).Value;
               elsif Eval (Lst.Values.Element
                             (Lst.Values.First_Index + 1), Env) in
                 P_Mal_Types.None_Data
               then
                  --  (if nil "yes" "no")
                  None_Found := True;
               end if;

               if None_Found or else Cond = False then
                  --  Eval "else body" if exist
                  if Positive (Lst.Values.Length) > 3 then
                     return Eval
                       (Lst.Values.Element (Lst.Values.First_Index + 3), Env);
                  else
                     return None;
                  end if;
               else
                  --  Eval "if body" if exist
                  return Eval
                    (Lst.Values.Element (Lst.Values.First_Index + 2), Env);
               end if;
            end;
         elsif Asu.To_String (Sbl.Value) = "fn*" then
            --  HACK: to fix "subprogram must not be deeper than access type"
            --  Ada compiler may thinks that Fn_Def is more "localy"
            --  than Eval function, and it's not
            --  One solution is to move Fn_Def type in this package
            --  (see hello_function_pointer2.adb for example)
            --  This instruction tells to Ada compiler OK it's good don't worry
            --  In some similare problems "Downward closures" may be used...
            Fn_Def.Eval := Eval'Unrestricted_Access;
            Fn_Def.Env := Env'Unrestricted_Access;
            if Lst.Values.Element (Lst.Values.First_Index + 1) in
              P_Mal_Types.Vector_Of_Data
            then
               Fn_Def.Args := P_Mal_Types.Vector_Of_Data
                 (Lst.Values.Element (Lst.Values.First_Index + 1)).Values;
            else
               Fn_Def.Args := P_Mal_Types.List_Of_Data
                 (Lst.Values.Element (Lst.Values.First_Index + 1)).Values;
            end if;
            Fn_Def.Bod.Append (Lst.Values.Element
                                 (Lst.Values.First_Index + 2));
            return Fn_Def;
         end if;
      end if;

      --  At the step, we don't found any symbol above
      --  So it's time to execute a user function or operator (also a symbol)
      --  List evaluation
      declare
         Lst_Args : P_Mal_Types.List_Of_Data;
         New_Env : P_Env.Env_Ref;
      begin
         Lst := P_Mal_Types.List_Of_Data (Eval_Ast (Ast, Env));

         if Lst.Values.Element (Lst.Values.First_Index)
           in P_Repl_Env.Fn_Def_Data
         then
            Fn_Def := P_Repl_Env.Fn_Def_Data
              (Lst.Values.Element (Lst.Values.First_Index));
            --  TODO: _slice function in C code
            Lst_Args.Values := Lst.Values.Copy;
            Lst_Args.Values.Delete_First;
            --  TODO: encapsulation is possible (move this new in Fn_)
            New_Env := new P_Env.Env'
              (P_Env.Init (Fn_Def.Env, Fn_Def.Args, Lst_Args));
            return Fn_Def.Eval
              (Fn_Def.Bod.Element (Fn_Def.Bod.First_Index), New_Env.all);
         else
            Fn_Op :=
              P_Repl_Env.Fn_Op_Data (Lst.Values.Element
                                       (Lst.Values.First_Index));
            Lst_Args.Values := Lst.Values.Copy;
            Lst_Args.Values.Delete_First;
            return Fn_Op.Value (Lst_Args);
         end if;
      end;
   end Eval;

   procedure Init_Env (Env : in out P_Env.Env) is
      Core_Ns : P_Repl_Env.Core_Ns.Map :=
        P_Repl_Env.Init_Core_Ns;
      Csr : P_Repl_Env.Core_Ns.Cursor :=
        P_Repl_Env.Core_Ns.First (Core_Ns);
   begin
      while P_Repl_Env.Core_Ns.Has_Element (Csr) loop
         Env.Set (P_Mal_Types.Symbol_Data'
                    (Value => P_Repl_Env.Core_Ns.Key (Csr)),
                  P_Repl_Env.Core_Ns.Element (Csr));
         P_Repl_Env.Core_Ns.Next (Csr);
      end loop;
      Env.Set (P_Mal_Types.Symbol_Data'
                 (Value => Asu.To_Unbounded_String ("eval")),
               P_Repl_Env.Fn_Op_Data'
                 (Value => Do_Eval'Unrestricted_Access));
   end Init_Env;

   function Print (Exp : P_Mal_Types.Data) return String is
   begin
      return P_Mal_Types.Pretty_Print (Exp, False, False, False);
   end Print;

   function Read (Str : String) return P_Mal_Types.Data is
   begin
      return P_Reader.Read_Str (Str);
   end Read;

   function Rep (Str : String; Env : in out P_Env.Env) return String is
   begin
      return Print (Eval (Read (Str), Env));
   end Rep;

   procedure Rep_No_Return (Str : String; Env : in out P_Env.Env) is
      Tmp : Asu.Unbounded_String;
   begin
      Tmp := Asu.To_Unbounded_String (Rep (Str, Env));
   end Rep_No_Return;

   Cmd_Line : Asu.Unbounded_String;
begin
   --  Print Version and Exit Information
   Ada.Text_IO.Put_Line ("Lispy Version 0.0.0.0.1");
   Ada.Text_IO.Put_Line ("Press Ctrl+d to Exit\n");

   Init_Env (G_Env);
   Rep_No_Return ("(def! not (fn* (a) (if a false true)))", G_Env);

   loop
      Try : begin
         Ada.Text_IO.Put ("lispy> ");
         exit when Ada.Text_IO.End_Of_File;
         Cmd_Line := Asu.To_Unbounded_String (Ada.Text_IO.Get_Line);
         if Ada.Command_Line.Argument_Count = 0 then
            Ada.Text_IO.Put (Rep (Asu.To_String (Cmd_Line), G_Env));
         elsif Ada.Command_Line.Argument (1) = "--pretty_print" then
            Ada.Text_IO.Put (Print (Read (Asu.To_String (Cmd_Line))));
         elsif Ada.Command_Line.Argument (1) = "--pretty_print_echo" then
            Ada.Text_IO.Put (Asu.To_String (Cmd_Line));
         end if;
            Ada.Text_IO.New_Line;
      exception
         when others =>
            Ada.Text_IO.Put ("Oups, something wrong happens :)");
            Ada.Text_IO.New_Line;
      end Try;
   end loop;
end Lispy;
