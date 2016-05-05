Ma_Sys.ma Relation Algebra Interpreter 1.0.0.0, Copyright (c) 2016 Ma_Sys.ma.
For further info send an e-mail to Ma_Sys.ma@web.de.

--------------------------------------------------------------[ Introduction ]--

The ``Relation Algebra Interpreter'' (or short RA Interpreter) is a program
which can read CSV tables and perform some basic relation algebra on it, that
is to say: Project, Join and Select operations.

This program was written for learning purposes. If you find it useful for
anything else, keep in mind, that it was probably not designed to do what you
want.

---------------------------------------------------------------[ Compilation ]--

If you are on a Linux system with the GNU Ada Compiler GNAT installed, it should
be enough to type `make` to build this program. Otherwise, you will need to
compile the program with any Ada 2012 compiler. The main file is
`ra_interpreter.adb`.

---------------------------------------------------------------------[ Usage ]--

The program runs interactively on the console and takes commands as input
and prints resulting tables directly to the console.

Format your files as comma separated value, for example like `test/t1.csv`

	File,Lines
	gpl.ads,24
	ra_interpreter.adb,26
	csv.ads,32
	func.ads,37

The first row is considered the schema of this relation which you can
interactively load as follows:

	RA> read(t1, test/t1.csv)

You will not get an answer, because you just load the relation into memory
under the name `t1` (first parameter to `read`).

In order to print a table, select all of its rows:

	RA> SEL[1](t1)
	ooooooooooooooooooooooooooooo
	  File                Lines  
	+++++++++++++++++++++++++++++
	  gpl.ads             24     
	  ra_interpreter.adb  26     
	  csv.ads             32     
	  func.ads            37     
	ooooooooooooooooooooooooooooo

It pretty-printed the relation.

Now, let's add a condition to the select statement:

	RA> SEL[(File = "csv.ads")](t1)
	oooooooooooooooooo
	  File     Lines  
	++++++++++++++++++
	  csv.ads  32     
	oooooooooooooooooo

Note the parenthes around the condition, we wrote `SEL[(...)]`. If you consult
what `help` has to say about this:

	RA> help
	[...]
	Bool:  (v = v)    Compares values.
	[...]

It says, that you have to put a comparison in parentheses This simplifies
parsing but is a bit more to write. Also, comparison only works for values.
You cannot compare Booleans with this program.

Finally, let's have a look at an example for Natural Joining. Load the relation
from `test/t2.csv` as `t2` and perform a join on the two tables:

	RA> load(t2, test/t2.csv)
	RA> JOIN(t1,t2)
	ooooooooooooooooooooooooooooooooooooooooo
	  File                Lines  Size/Bytes  
	+++++++++++++++++++++++++++++++++++++++++
	  gpl.ads             24     858         
	  ra_interpreter.adb  26     1038        
	  csv.ads             32     1126        
	  func.ads            37     1400        
	ooooooooooooooooooooooooooooooooooooooooo

... It works as expected joining the two relations on the common field `File`.

Explore the other options by viewing `help`. Most of the work as expected.

--------------------------------------------------------------------[ Issues ]--

As this was created for learning purposes, it's features are only rudimentary.
Especially, you can not

 - Recall commands via a history
 - Use another syntax than given there, spaces are sometimes relevant!
 - Do a lot of boolean operations
 - Use fields longer than 256 characters :(

Programming Language
   This was written in Ada as a means of getting to know that language.
   Thus, there are probably a lot of beginner's mistakes in there...
