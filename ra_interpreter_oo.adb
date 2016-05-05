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

with Ada.Text_IO;
use  Ada.Text_IO;
with Ada.Exceptions;
use  Ada.Exceptions;
with GPL;
with Query_DB;
use  Query_DB;

package body RA_Interpreter_OO is

	procedure RA_Interpreter_OO_Main is
		Active: Boolean := True;
		D: Database;
	begin
		Info;
		D := Create_Database;
		while Active loop
			Put("RA> ");
			Flush;
			declare
				Query: String := Get_Line;
			begin
				Active := Active and Process_Query(D, Query);
			end;
		end loop;
	exception
		when E: End_Error => null;
	end RA_Interpreter_OO_Main;

	procedure Info is
	begin
		Put_Line("Ma_Sys.ma Relation Algebra Interpreter 1.0.0.0, " &
					"Copyright (c) 2016 Ma_Sys.ma.");
		Put_Line("For further info send an e-mail to " &
							"Ma_Sys.ma@web.de.");
		New_Line;
		Put_Line("Type `help` to list commands. Type `gpl` to view " &
								"license.");
		New_Line;
	end Info;

	function Process_Query(D: in out Database; Q: in String)
							return Boolean is
		Sep_L, Sep_R: Natural;
	begin
		Locate_Separator(Q, Sep_L, Sep_R);
		declare
			Cmd: String := Q(Q'First .. Sep_L - 1);
		begin
			if Sep_L = 0 then -- query too short is ignored.
				return True;
			end if;

			if Cmd = "exit" or Cmd = "q" or Cmd = "quit" or
								Cmd = ".q" then
				return False;
			elsif Cmd = "help" or Cmd = "?" or Cmd = "h" then
				Help;
			elsif Cmd = "gpl" or Cmd = "lic" or Cmd = "!" then
				License;
			elsif Cmd = "read" or Cmd = "load" then
				Read_Table_Param(D, Q(Sep_R + 1 .. Q'Last));
			else
				Query_DB_And_Print(D, Q);
			end if;
		exception
			when E: others =>
				Put_Line("Error occurred: " &
						Exception_Information(E));
		end;
		return True;
	end Process_Query;

	procedure Locate_Separator(S: in String; L, R: out Natural) is
	begin
		L := 0;
		R := 0;
		for I in S'Range loop
			if S(I) = ' ' or S(I) = '[' or S(I) = '(' or
						S(I) = ')' or S(I) = ']' or
						S(I) = ',' then
				if L = 0 then
					L := I;
					R := L;
				else
					R := R + 1;
				end if;
			elsif L /= 0 then
				exit;
			end if;
		end loop;
		if L = 0 then
			L := S'Last + 1;
			R := L;
		end if;
	end Locate_Separator; 

	procedure Help is
	begin
		Put_Line("Statements (cannot be nested)");
		Put_Line("help              Displays this help.");
		Put_Line("gpl               Displays license (GPL 3).");
		Put_Line("read(name, file)  Loads a table from a CSV file.");
		Put_Line("                  Spaces in file and table names " &
							"are not permitted.");
		New_Line;
		Put_Line("Queries");
		Put_Line("UNION(R,S)        Computes the union of two tables.");
		Put_Line("PRJ[A1,...](R)    Projects table R preserving " &
							"columns A1...");
		Put_Line("SEL[F*](R)        Selects rows from R by bool ");
		Put_Line("                  filter function F* (bool).");
		Put_Line("JOIN(R,S)         Performs a natural join of " &
								" R and S.");
		New_Line;
		Put_Line("Selection Functions (constructs for F*)");
		Put_Line("Value: ""val""      Represents a constant Value.");
		Put_Line("Value: C          Represents the value of the " &
							"column with name C.");
		Put_Line("Bool:  1          Bool expression constant true.");
		Put_Line("Bool:  0          Bool expression constant false.");
		Put_Line("Bool:  (v = v)    Compares values.");
		Put_Line("Bool:  (b & b)    Logical and.");
		Put_Line("Bool:  (b | b)    Logical or.");
		Put_Line("Bool:  (!b)       Logical not.");
		null;
	end;

	procedure License is
		use GPL;
	begin
		Print_GPL;
	end;

	procedure Read_Table_Param(D: in out Database; P: in String) is
		Sep_L, Sep_R: Natural;
	begin
		Locate_Separator(P, Sep_L, Sep_R);
		declare
			Sep_L2, Sep_R2: Natural;
			File_Name: String := P(Sep_R + 1 .. P'Last);
		begin
			Locate_Separator(File_name, Sep_L2, Sep_R2);
			Add_Table(D, Read_Table(File_Name(File_Name'First ..
					Sep_L2 - 1)), P(P'First .. Sep_L - 1));
		end;
	end;

end RA_Interpreter_OO;
