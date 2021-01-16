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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;
with P_Mal_Types;
with P_Env;

package P_Repl_Env is
   package Asu renames Ada.Strings.Unbounded;

   function Read_String (Args : P_Mal_Types.Data)
                     return P_Mal_Types.Data;

   function List (Args : P_Mal_Types.Data)
                 return P_Mal_Types.Data;

   --  TODO: this function can be moved in order to use it
   --  in some part of code
   --  This function is _list in C code
   function List_Q (Args : P_Mal_Types.Data)
                   return P_Mal_Types.Data;

   --  TODO: this function can be moved in order to use it
   --  in some part of code
   function Empty_Q (Args : P_Mal_Types.Data)
                   return P_Mal_Types.Data;

   function Count (Args : P_Mal_Types.Data)
                   return P_Mal_Types.Data;

   function Prn (Args : P_Mal_Types.Data)
                return P_Mal_Types.Data;

   function Pr_Str (Args : P_Mal_Types.Data)
                   return P_Mal_Types.Data;

   function Do_Str (Args : P_Mal_Types.Data)
                   return P_Mal_Types.Data;

   function Println (Args : P_Mal_Types.Data)
                    return P_Mal_Types.Data;

   function Plus (Args : P_Mal_Types.Data)
                 return P_Mal_Types.Data;

   function Min (Args : P_Mal_Types.Data)
                return P_Mal_Types.Data;

   function Mult (Args : P_Mal_Types.Data)
                 return P_Mal_Types.Data;

   function Div (Args : P_Mal_Types.Data)
                return P_Mal_Types.Data;

   function Equal (Args : P_Mal_Types.Data)
                  return P_Mal_Types.Data;

   function Sup (Args : P_Mal_Types.Data)
                return P_Mal_Types.Data;

   function Sup_Equal (Args : P_Mal_Types.Data)
                      return P_Mal_Types.Data;

   function Inf (Args : P_Mal_Types.Data)
                return P_Mal_Types.Data;

   function Inf_Equal (Args : P_Mal_Types.Data)
                      return P_Mal_Types.Data;

   type Fn_Op_Data is new P_Mal_Types.Atomic with record
      Value : access function (Args : P_Mal_Types.Data)
                              return P_Mal_Types.Data;
   end record;

   type Fn_Def_Data is new P_Mal_Types.Atomic with record
      Eval : access function (Ast : P_Mal_Types.Data; Env : in out P_Env.Env)
                             return P_Mal_Types.Data;
      Env : P_Env.Env_Ref;
      --  HACK: with vector type it's possible to handle both List and Vector
      Args : P_Mal_Types.Vectors.Vector;
      --  HACK: avoid to handle memory with a pointer
      Bod : P_Mal_Types.Vectors.Vector;
   end record;

   function "<" (Left, Right : Asu.Unbounded_String) return Boolean;

   package Core_Ns is new Ada.Containers.Indefinite_Ordered_Maps
     (Asu.Unbounded_String, Fn_Op_Data);
   function Init_Core_Ns return Core_Ns.Map;
end P_Repl_Env;
