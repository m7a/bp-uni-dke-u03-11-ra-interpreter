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

with Ada.Strings.Bounded;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with T_Line;
use  T_Line;

package DB is

	Query_Failure: exception;

	type Database is private;
	type Table    is tagged private;

	-- Database operations
	function Create_Database return Database;
	procedure Add_Table(D: in out Database; T: in Table; Name: in String);
	function Get_Table(D: in Database; N: String) return Table;

	-- Table operations
	function Read_Table(File: in String) return Table;
	function Union(R, S: in Table) return Table; -- UNION(R,S)
	function Project(R: in Table; H: in Line)
						return Table; -- PRJ[H...](R)
	function Select_By_Function(R: in Table; F: in String) return Table;
	function Join_Natural(R, S: in Table) return Table; -- JOIN(R,S)

	procedure Print_Table(T: in Table);

	function "="(a, b: in Table) return Boolean;

private

	procedure Insert_If_Not_Present(T: in out Table; L: in Line);

	-- Table
	package Table_Lines is new Ada.Containers.Indefinite_Vectors(Natural,
									Line);
	use Table_Lines;

	type Table is tagged record
		T: Table_Lines.Vector;
	end record;

	-- Database
	package DB_Map is new Ada.Containers.Indefinite_Ordered_Maps(String,
									Table);

	type Database is record
		Tables: DB_Map.Map;
	end record;

end DB;
