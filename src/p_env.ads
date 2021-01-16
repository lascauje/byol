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

with Ada.Finalization; use Ada.Finalization;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Unchecked_Deallocation;
with P_Mal_Types; use P_Mal_Types;

package P_Env is
   type Env is tagged private;
   type Env_Ref is access Env;
   function Init (Outer : Env_Ref; Binds : P_Mal_Types.Vectors.Vector;
                  Exprs : P_Mal_Types.List_Of_Data) return Env;
   function Init (Outer : Env) return Env;
   function Init return Env;
   --  RAII method
   procedure Finalize (Self : in out Env);
   function Find (Self : Env; Key : Symbol_Data) return Env;
   procedure Set (Self : in out Env; Key : Symbol_Data; Value : Data);
   function Set_Ret (Self : in out Env; Key : Symbol_Data; Value : Data)
                    return Data;
   function Get (Self : Env; Key : Symbol_Data) return Data;
private
   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Symbol_Data, Data);
   type Env is new Controlled with record
      Data : Maps.Map;
      Outer_Ref : Env_Ref := null;
   end record;
   --  procedure Free_Env is new
   --    Ada.Unchecked_Deallocation (Env, Env_Ref);
end P_Env;
