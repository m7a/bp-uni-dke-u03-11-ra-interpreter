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
use  Ada.Strings.Fixed;
with Ada.Text_IO;
with T_Line;
use  T_Line;
with CSV;

package body Query_DB is

	Expected_Named_Query:                      exception;
	Expected_Parametrized_Query:               exception;
	Misformatted_Named_Query_Arguments:        exception;
	Misformatted_Parametrized_Query_Arguments: exception;

	procedure Query_DB_And_Print(D: in Database; Q: in String) is
		use Ada.Text_IO;
	begin
		Print_Table(Evaluate(D, Q));
	exception
		when Expected_Named_Query =>
			Put_Line("Error: Expected named query like UNION or " &
									"JOIN");
		when Expected_Parametrized_Query =>
			Put_Line("Error: Expected parametrized query like " &
								"PRJ or SEL");
		when Misformatted_Parametrized_Query_Arguments =>
			Put_Line("Error: Parametrized query's arguments are " &
						"incorrectly formatted");
		when Misformatted_Named_Query_Arguments =>
			Put_Line("Error: Query Parameters for UNION or JOIN " &
						"are incorrectly formatted.");
	end Query_DB_And_Print;

	function Evaluate(D: in Database; Q: in String) return Table is
		First_Open: Natural := Locate_First_Open(Q);
	begin
		if First_Open = 0 then
			-- recursion anchor... Q is now a table name
			return Get_Table(D, Q);
		end if;

		declare
			First_Bra: Natural := Index(Q, "[", Q'First);
			Rest:      String  := Q(First_Open .. Q'Last);
		begin
			if First_Bra = 0 then
				return Ev_Named_Query(D, Q(Q'First ..
							First_Open - 1), Rest);
			else
				return Ev_Parametrized_Query(
					D,
					Q(Q'First       .. First_Bra - 1),
					Q(First_Bra + 1 .. First_Open - 2),
					Q(First_Open    .. Q'Last)
				);
			end if;
		end;
	end Evaluate;

	function Locate_First_Open(Q: in String) return Natural is
		Level: Natural := 0;
	begin
		for I in Q'Range loop
			if Q(I) = '[' then
				Level := Level + 1;
			elsif Q(I) = ']' then
				Level := Level - 1;
			elsif Level = 0 and Q(I) = '(' then
				return I;
			end if;
		end loop;
		return 0;
	end Locate_First_Open;

	-- Union, Join
	function Ev_Named_Query(D: in Database;
				Name, Rest: in String) return Table is
		O: Named_Query := Identify_Named_Query(Name);
		High_Level_Comma, High_Level_End: Natural := 0;

		procedure Update_Comma(C: in Character; I, Level: in Natural) is
		begin
			if Level = 1 and C = ',' then
				High_Level_Comma := I;
			end if;
		end;
	begin
		-- incidentally, all functions in this category have arity 2
		Locate_Close(Rest, High_Level_End, Update_Comma'Access);
		if High_Level_Comma = 0 or High_Level_End = 0 then
			raise Misformatted_Named_Query_Arguments;
		end if;
		declare
			T1: Table := Evaluate(D, Rest(Rest'First + 1 ..
							High_Level_Comma - 1));
			T2: Table := Evaluate(D, Rest(High_Level_Comma + 1 ..
							High_Level_End - 1));
		begin
			case O is
			when Op_Union => return Union(T1, T2);
			when Op_Join  => return Join_Natural(T1, T2);
			end case;
		end;
	end Ev_Named_Query;

	procedure Locate_Close(S: in String; High_Level_End: out Natural;
					R: access procedure(C: in Character;
					I, Level: in Natural)) is
		Level: Natural := 0;
		Current: Character;
	begin
		High_Level_End := 0;
		for I in S'Range loop
			Current := S(I);
			if Current = '(' then
				Level := Level + 1;
			elsif Current =')' then
				Level := Level - 1;
				if Level = 0 then
					High_Level_End := I;
					exit;
				end if;
			else
				R(Current, I, Level);
			end if;
		end loop;
	end;

	function Identify_Named_Query(Name: in String) return Named_Query is
	begin
		if Name = "union" or Name = "UNION" or Name = "U" then
			return Op_Union;
		elsif Name = "join" or Name = "JOIN" or Name = "J" then
			return Op_Join;
		else
			raise Expected_Named_Query;
		end if;
	end Identify_Named_Query;

	-- Project, Select
	function Ev_Parametrized_Query(D: in Database;
				Name, Param, Rest: in String) return Table is
		O: Parametrized_Query := Identify_Parametrized_Query(Name);
		High_Level_End: Natural := 0;

		procedure Null_Update(C: in Character; I, Level: in Natural) is
									null;
	begin
		-- incidentally, all functions in this category have arity 1
		Locate_Close(Rest, High_Level_End, Null_Update'Access);
		if High_Level_End = 0 then
			raise Misformatted_Parametrized_Query_Arguments;
		end if;
		declare
			T: Table := Evaluate(D, Rest(Rest'First + 1 ..
							High_Level_End - 1));
		begin
			case O is
			when Op_Select  => return Select_By_Function(T, Param);
			when Op_Project => return Project_Raw(T, Param);
			end case;
		end;
	end Ev_Parametrized_Query;

	function Identify_Parametrized_Query(Name: in String)
						return Parametrized_Query is
	begin
		if Name = "PRJ" or Name = "project" then
			return Op_Project;
		elsif Name = "SEL" or Name = "select" then
			return Op_Select;
		else
			raise Expected_Parametrized_Query;
		end if;
	end Identify_Parametrized_Query;

	function Project_Raw(T: in Table; Raw: in String) return Table is
		W: Natural := 0;
		L_Proj: Line := CSV.Process_Line(Raw, W);
	begin
		return T.Project(L_Proj);
	end;

end Query_DB;
