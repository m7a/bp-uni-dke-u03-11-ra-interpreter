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

with T_Line;
use  T_Line;

package Func is

	function Matches(F: String; L: in Line; H: in Line) return Boolean;

private

	type Operator is (Op_Not, Op_And, Op_Or, Op_Value_Eq);
	procedure Locate_Operator(F_String: in String; Op_Loc: out Natural;
							Op_Type: out Operator);
	function Proc_Binary_Op(F: String; L, H: Line; Op_Loc: Natural;
					C_Op: Operator) return Boolean;
	function Value(F: String; L, H: Line) return String;
	function Trim(S: in String) return String;

end Func;
