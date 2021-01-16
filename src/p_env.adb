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
with Ada.Text_IO;

package body P_Env is
   package Asu renames Ada.Strings.Unbounded;
   overriding
   procedure Finalize (Self : in out Env) is
   begin
      null;
      --  Useless because Ada auto free attributes object...
      --  if Self.Outer_Ref /= null then
      --     Free_Env (Self.Outer_Ref);
      --  end if;
   end Finalize;

   function Find (Self : Env; Key : Symbol_Data) return Env is
      Csr : Maps.Cursor := Maps.First (Self.Data);
      Key_Error : exception;
      Msg_Error : Asu.Unbounded_String;
   begin
      --  TODO: use "contains" function instead
      while Maps.Has_Element (Csr) loop
         if Symbol_Data (Maps.Key (Csr)) = Key then
            return Self;
         end if;
         Maps.Next (Csr);
      end loop;
      if Self.Outer_Ref /= null then
         return Self.Outer_Ref.all.Find (Key);
      else
         Msg_Error := Asu.To_Unbounded_String
           ("key '" & Asu.To_String (Key.Value) & "' not found");
         Ada.Text_IO.Put_Line (Asu.To_String (Msg_Error));
         raise Key_Error
           with Asu.To_String (Msg_Error);
      end if;
   end Find;

   function Get (Self : Env; Key : Symbol_Data) return Data is
      Env_Key : Env := Self.Find (Key);
   begin
      return Env_Key.Data.Element (Key);
   end Get;

   function Init (Outer : Env_Ref; Binds : P_Mal_Types.Vectors.Vector;
                  Exprs : P_Mal_Types.List_Of_Data) return Env is
      New_Env : Env := Init;
      Ind : Positive := 1;
      Lst_Args : P_Mal_Types.List_Of_Data;
   begin
      New_Env.Outer_Ref := Outer;
      while Ind <= Positive (Binds.Length) loop
         if Asu."=" (P_Mal_Types.Symbol_Data (Binds.Element (Ind)).Value, "&")
         then
            --  TODO: _slice function could be used like in C code
            Lst_Args.Values := Exprs.Values.Copy;
            Lst_Args.Values.Delete_First (Ada.Containers.Count_Type (Ind - 1));
            New_Env.Data.Insert (P_Mal_Types.Symbol_Data
                                   (Binds.Element (Ind + 1)),
                                 Lst_Args);
            exit;
         end if;
         New_Env.Data.Insert (P_Mal_Types.Symbol_Data (Binds.Element (Ind)),
                              Exprs.Values.Element (Ind));
         Ind := Ind + 1;
      end loop;
      return New_Env;
   end Init;

   function Init (Outer : Env) return Env is
      New_Env : Env := Init;
   begin
      New_Env.Outer_Ref := new Env'(Outer);
      return New_Env;
   end Init;

   function Init return Env is
      New_Env : Env;
   begin
      return New_Env;
   end Init;

   procedure Set (Self : in out Env; Key : Symbol_Data; Value : Data) is
   begin
      --  TODO: use "include" function instead
      Self.Data.Insert (Key, Value);
   exception
      when Constraint_Error => Self.Data.Replace (Key, Value);
   end Set;

   function Set_Ret (Self : in out Env; Key : Symbol_Data; Value : Data)
                    return Data is
   begin
      Self.Set (Key, Value);
      return Value;
   end Set_Ret;
end P_Env;
