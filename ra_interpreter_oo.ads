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

package RA_Interpreter_OO is

	procedure RA_Interpreter_OO_Main;

private

	procedure Info;
	function Process_Query(D: in out Database; Q: in String) return Boolean;
	procedure Locate_Separator(S: in String; L, R: out Natural);

	procedure Help;
	procedure License;
	procedure Read_Table_Param(D: in out Database; P: in String);

end RA_Interpreter_OO;
