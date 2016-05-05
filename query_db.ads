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

with DB;
use  DB;

package Query_DB is

	procedure Query_DB_And_Print(D: in Database; Q: in String);

private

	type Named_Query is (Op_Union, Op_Join);
	type Parametrized_Query is (Op_Project, Op_Select);

	function Evaluate(D: in Database; Q: in String) return Table;
	function Locate_First_Open(Q: in String) return Natural;
	function Ev_Named_Query(D: in Database; Name, Rest: in String)
								return Table;
	procedure Locate_Close(S: in String; High_Level_End: out Natural;
					R: access procedure(C: in Character;
					I, Level: in Natural));
	function Identify_Named_Query(Name: in String) return Named_Query;
	function Ev_Parametrized_Query(D: in Database;
				Name, Param, Rest: in String) return Table;
	function Identify_Parametrized_Query(Name: in String)
						return Parametrized_Query;
	function Project_Raw(T: in Table; Raw: in String) return Table;

end Query_DB;
