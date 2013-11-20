CUTE-homework
=============

This is part of the homework assignment described [here](http://fa09.pbworks.com/w/page/4233620/CUTE-Homework).

The goal was to implement a test case generation tool similar to CUTE/DART for a
subset of C.

The tool was implemented in OCaml. We prefered to use the STP solver instead of
the recommended yices however we would like to add support for more solvers.


Note that we used STP and it's OCaml bindings found
[here](http://www.cs.umd.edu/class/spring2013/cmsc631/p2.shtml) not the ones from
STP's site.

**Dependencies**

* [Assignment files](http://srl.cs.berkeley.edu/%7Eksen/assignment.tar.gz)
* [STP](http://www.cs.umd.edu/class/spring2013/cmsc631/p2/p2-updated-stp.zip) 

**To compile:**

    $ cd stp
    $ ./clean-install.sh
    $ cd ../
    $ cd cil
    $ ./configure
    $ make
    $ cd cute
    $ make
    $ cd mycute
    $ make

**To test:**

    $ cd cute
    $ ./cutec tests/testme.c
    $ tests/testme.exe
    $ ./cute ../mycute/mycute tests/testme.exe -i 10

