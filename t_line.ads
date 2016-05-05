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

package T_Line is

	Max_Field_Size: constant := 256;

	package LBS is new Ada.Strings.Bounded.Generic_Bounded_Length(
								Max_Field_Size);
	use LBS;

	type Line is array(Positive range <>) of Bounded_String;

	function LMK(S: in String) return Bounded_String is
						(LBS.To_Bounded_String(S));
	function Len(B: in Bounded_String) return Natural is (Length(B));
	function Get(L: in Line; I: Natural) return Bounded_String is (L(I));
	function Get(L: in Line; I: Natural) return String is
						(LBS.To_String(L(I)));

end T_Line;
