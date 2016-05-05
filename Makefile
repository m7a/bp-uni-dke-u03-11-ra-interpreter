#    Ma_Sys.ma Relation Algebra Interpreter Program
#    Copyright (c) 2016 Ma_Sys.ma.
#    For further info send an e-mail to Ma_Sys.ma@web.de.
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    Notice: You find this copy by interactively typing `gpl` or in `gpl.adb`

all:
	gnatmake -gnatwa -gnatwK -O3 -j8 ra_interpreter.adb

test: test-join2

test-join2: all
	( echo load a test/tbl_a.csv; echo load b test/tbl_b.csv; \
					echo "JOIN(a,b)" ) | ./ra_interpreter

test-project: all
	( echo load test test/test_tbl.csv; \
			echo "project[C,A,A,B,B,B](test)" ) | ./ra_interpreter

test-join: all
	( echo load test test/test_tbl.csv; echo load test2 test/test_tbl.csv; \
				echo "JOIN(test,test2)") | ./ra_interpreter

test-interactive: all
	./ra_interpreter

clean:
	-rm *.ali *.o

dist-clean: clean
	-rm ra_interpreter

# Probably only works on Debian systems
gengpl:
	cat /usr/share/common-licenses/GPL-3 | while read -r line; do \
		if [ -z "$$line" ]; then \
			echo "New_Line;"; \
		else \
			echo "Put_Line(\"$$(echo "$$line" | \
						sed 's/\"/\"\"/g')\");"; \
		fi; \
	done

todo:
	grep --color=auto -RF TO''DO .

# Requires Ma_Sys.ma Big 4 -> https://masysma.lima-city.de/33/big_4.xhtml
#                             https://masysma.lima-city.de/33/big.xhtml
test-mem: all
	big4 -t /tmp/test.csv 1024 KiB
	( echo load test /tmp/test.csv; echo load test2 /tmp/test.csv; \
		while true; do \
			echo "JOIN(test,test2)"; \
		done ) | ./ra_interpreter
	rm /tmp/test.csv
