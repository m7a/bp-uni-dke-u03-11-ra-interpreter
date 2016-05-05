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

package body CSV is

	function Process_Line(Raw: in String; Width: in out Natural)
								return Line is
		Pos_Prev: Natural;
		Pos: Natural := Raw'First;
		L_Idx: Positive := 1;
	begin
		if Width = 0 then
			Width := Count(Raw, Comma) + 1;
		end if;
		declare
			L: Line(1 .. Width);
		begin
			while Pos <= Raw'Last loop
				Pos_Prev := Pos;
				Pos := Index(Raw, Comma, Pos);
				if Pos = 0 then
					Pos := Raw'Last + 1;
				end if;
				if L_Idx > Width then
					raise CSV_More_Fields_Than_Header_Cols;
				end if;
				L(L_Idx) := LMK(Raw(Pos_Prev .. Pos - 1));
				Pos   := Pos + 1;
				L_Idx := L_Idx + 1;
			end loop;
			return L;
		end;
	end Process_Line;
	
end CSV;
