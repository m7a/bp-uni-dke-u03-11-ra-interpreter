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
with Ada.Strings.Fixed;

with CSV;
with Func;

package body DB is

	Outer_Sep:     constant Character := 'o';  -- alt.: -
	Inner_Sep:     constant Character := '+';  -- alt.: =
	Use_Outer_Sep: constant Boolean   := True;
	Padding:       constant           := 2;    -- alt.: 0
	Field_Pad:     constant           := 2;

	----------------------------------------------------------[ Database ]--

	function Create_Database return Database is
		Ret: Database := (Tables => DB_Map.Empty_Map);
	begin
		return Ret;
	end;

	procedure Add_Table(D: in out Database; T: in Table; Name: in String) is
	begin
		if D.Tables.Contains(Name)  then
			Ada.Text_IO.Put_Line("The table name """ & Name &
						""" is already in use.");
		else
			D.Tables.Insert(Name, T);
		end if;
	end Add_Table;

	function Get_Table(D: in Database; N: in String) return Table is
	begin
		if D.Tables.Contains(N) then
			return D.Tables.Element(N);
		else
			raise Query_Failure with "Table not found: " & N;
		end if;
	end;

	------------------------------------------------------------[ Tables ]--

	function "="(a, b: in Table) return Boolean is
	begin
		return a.T = b.T;
	end "=";

	function Read_Table(File: in String) return Table is
		use Ada.Text_IO;
		Width: Natural := 0;
		Ret: Table := (T => Table_Lines.Empty_Vector);
		Handle: File_Type;
	begin
		Open(Handle, In_File, File);
		while not End_Of_File(Handle) loop
			declare
				Raw: String := Get_Line(Handle);
			begin
				Ret.T.Append(CSV.Process_Line(Raw, Width));
			end;
		end loop;
		Close(Handle);
		return Ret;
	end Read_Table;

	function Union(R, S: in Table) return Table is
		Ret: Table := (T => Table_Lines.Empty_Vector);
	begin
		for El_R of R.T loop
			Ret.Insert_If_Not_Present(El_R);
		end loop;
		for El_S of S.T loop
			Ret.Insert_If_Not_Present(El_S);
		end loop;
		return Ret;
	end Union;

	procedure Insert_If_Not_Present(T: in out Table; L: in Line) is begin
		if T.T.Find(L) = No_element then
			T.T.Append(L);
		end if;
	end Insert_If_Not_Present;

	function Project(R: in Table; H: in Line) return Table is
		Select_Indices: array (H'Range) of Natural;
		Header_R: Line := R.T(0);
		Ret: Table := (T => Table_Lines.Empty_Vector);
	begin
		for I in Header_R'Range loop
			for K in H'Range loop
				if Get(Header_R, I) = Get(H, K) then
					Select_Indices(K) := I;
				end if;
			end loop;
		end loop;
		for L of R.T loop
			declare
				NL: Line(H'Range);
			begin
				for I in H'Range loop
					NL(I) := Get(L, Select_Indices(I));
				end loop;
				Ret.T.Append(NL);
			end;
		end loop;
		return Ret;
	end Project;

	function Select_By_Function(R: in Table; F: in String) return Table is
		Ret: Table := (T => Table_Lines.Empty_Vector);
		Header: Line := R.T(0);
		First: Boolean := True;
	begin
		for L of R.T loop
			if First then
				First := False;
				Ret.T.Append(L); -- Copy header (pointers)
			else
				if Func.Matches(Ada.Strings.Fixed.Trim(F,
							Ada.Strings.Both),
							L, Header) then 
					Ret.T.Append(L); -- COPY POINTERS
				end if;
			end if;
		end loop;
		return Ret;
	end Select_By_Function;

	-- TODO z messy
	function Join_Natural(R, S: in Table) return Table is
		Common_Idx_R, Common_Idx_S: Natural := 0;
		Ret: Table := (T => Table_Lines.Empty_Vector);
		-- -1 for common column
		Titles_R: Line := R.T(0);
		Titles_S: Line := S.T(0);
		New_Width: Natural := Titles_R'Length + Titles_S'Length - 1;
		Is_First: Boolean := True;

		procedure Append_Joined(L_Proto, CL_I: in Line;
							L_Idx: in Natural) is
			L_New: Line := L_Proto; -- Copy (COPIES POINTERS!)
			L_Idx_New: Natural := L_Idx;
		begin
			for Field_Idx in CL_I'Range loop
				if Field_Idx /= Common_Idx_S then
					L_New(L_Idx_New) := Get(CL_I,
								Field_Idx);
					L_Idx_New := L_Idx_New + 1;
				end if;
			end loop;
			Ret.T.Append(L_New);
		end Append_Joined;
	begin
		for I in Titles_R'Range loop
			for J in Titles_S'Range loop
				if Get(Titles_R, I) = Get(Titles_S, J) then
					Common_Idx_R := I;
					Common_Idx_S := J;
					exit;
				end if;
			end loop;
		end loop;
		if Common_Idx_R = 0 then
			raise Query_Failure with "The given tables do not " &
							"share an attribute.";
		end if;
		for CL_O of R.T loop
			declare
				L_Proto: Line(1 .. New_Width);
				L_Idx: Natural := 1;
			begin
				-- Copy current line duplicating memory
				for Field of CL_O loop
					L_Proto(L_Idx) := Field;
					L_Idx := L_Idx + 1;
				end loop; 

				for CL_I of S.T loop
					if Get(CL_O, Common_Idx_R) = Get(CL_I,
							Common_Idx_S) then
						Append_Joined(L_Proto, CL_I,
									L_Idx);
						-- Prevent first line from
						-- being joined
						if Is_First then
							Is_First := False;
							exit;
						end if;
					end if;
				end loop;
			end;
		end loop;

		return Ret;
	end Join_Natural;

	procedure Print_Table(T: in Table) is
		Header: Line := T.T(0);
		TW: array (Header'Range) of Natural := (others => 0);
		TWS: Natural := Padding;
		Print_Sep: Boolean := True;
		procedure Print_Outer is
		begin
			if Use_Outer_Sep then
				Ada.Text_IO.Put_Line(Ada.Strings.Fixed."*"(TWS,
								Outer_Sep));
			end if;
		end Print_Outer;
	begin
		-- determine width
		for L of T.T loop
			for I in L'Range loop
				TW(I) := Natural'Max(TW(I), Len(L(I)));
			end loop;
		end loop;

		for I in TW'Range loop
			TWS := TWS + TW(I) + Field_Pad;
		end loop;
		TWS := TWS - Field_Pad + Padding;

		-- print
		Print_Outer;
		for L of T.T loop
			Ada.Text_IO.Put(Ada.Strings.Fixed."*"(Padding, ' '));
			for I in L'Range loop
				Ada.Text_IO.Put(Get(L, I));
				Ada.Text_IO.Put(Ada.Strings.Fixed."*"(TW(I) -
						Len(L(I)) + Field_Pad, ' '));
			end loop;
			Ada.Text_IO.New_Line;
			if Print_Sep then
				Ada.Text_IO.Put_Line(Ada.Strings.Fixed."*"(TWS,
								Inner_Sep));
				Print_Sep := False;
			end if;
		end loop;
		Print_Outer;
	end Print_Table;

end DB;
