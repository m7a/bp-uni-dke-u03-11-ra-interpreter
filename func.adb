--    Ma_Sys.ma Relation Algebra Interpreter Program
--    Copyright (c) 2016 Ma_Sys.ma.
--    For further info send an e-mail to Ma_Sys.ma@web.de.
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
--    Notice: You find this copy by interactively typing `gpl` or in `gpl.adb`

with Ada.Strings.Fixed;
with Ada.Strings;

package body Func is

	Matching_Function_Too_Short:    exception;
	Misformatted_Matching_Function: exception;
	Matching_Error:                 exception;

	function Matches(F: String; L, H: in Line) return Boolean is
		Op_Loc: Natural;
		C_Op: Operator;
	begin
		if F'Length = 0 then
			raise Matching_Function_Too_Short;
		end if;
		if F(F'First) = '(' then -- recurse
			Locate_Operator(F, Op_Loc, C_Op);
			if C_Op = Op_Not then
				return not Matches(Trim(F(Op_Loc + 1 ..
							F'Last - 1)), L, H);
			else
				return Proc_Binary_Op(F, L, H, Op_Loc, C_Op);
			end if;
		elsif F(F'First) = '1' then -- anchor
			return True;
		elsif F(F'First) = '0' then -- anchor
			return False;
		else
			raise Misformatted_Matching_Function with
				"Matching functions can only be bool " &
				"expressions and thus need to either " &
				"start with (, 0 or 1";
		end if;
	end Matches;

	function Trim(S: in String) return String is
	begin
		return Ada.Strings.Fixed.Trim(S, Ada.Strings.Both);
	end Trim;

	procedure Locate_Operator(F_String: in String; Op_Loc: out Natural;
						Op_Type: out Operator) is
		Level: Natural := 0;
		In_String: Boolean := False;
		C: Character;
	begin
		for Index in F_String'Range loop
			C := F_String(Index);
			if C = '"' then
				In_String := not In_String;
			elsif not In_String then
				if C = '(' then
					Level := Level + 1;
				elsif C = ')' then
					Level := Level - 1;
				elsif Level = 1 and (C = '!' or C = '&' or
							C = '|' or C = '=') then
					Op_Loc := Index;
					case C is
					when '!'    => Op_Type := Op_Not;
					when '&'    => Op_Type := Op_And;
					when '|'    => Op_Type := Op_Or;
					when '='    => Op_Type := Op_Value_Eq;
					when others => raise Constraint_Error;
					end case;
					return;
				end if;
			end if;
		end loop;
		raise Misformatted_Matching_Function with "No operator found " &
							"in function String.";
	end Locate_Operator;

	function Proc_Binary_Op(F: String; L, H: Line; Op_Loc: Natural;
					C_Op: Operator) return Boolean is
		Arg_L: String := Trim(F(F'First + 1 .. Op_Loc - 1));
		Arg_R: String := Trim(F(Op_Loc + 1 .. F'Last - 1));
	begin
		if C_Op = Op_Value_Eq then
			return Value(Arg_L, L, H) = Value(Arg_R, L, H);
		else
			declare
				Bool_L: Boolean := Matches(Arg_L, L, H);
				Bool_R: Boolean := Matches(Arg_R, L, H);
			begin
				case C_Op is
				when Op_And => return Bool_L and Bool_R;
				when Op_Or  => return Bool_L or  Bool_R;
				when others => raise Constraint_Error with
					"Unreachable code reached. Program bug";
				end case;
			end;
		end if;
	end;

	function Value(F: String; L, H: Line) return String is
	begin
		if F(F'First) = '"' then
			return F(F'First + 1 .. F'Last - 1);
		else
			for I in H'Range loop
				if Get(H, I) = F then
					return Get(L, I);
				end if;
			end loop;
			raise Matching_Error with "Column not found: " & F;
		end if;
	end;

end Func;
