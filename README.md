favabeans
=====

An OTP application that dumps the contents of processes to the disk every second or every few seconds. Useful to see the state of the system before it crashes

Build
-----

    $ rebar3 compile


Start
-----

	> application:start(favabeans).

Inspect
-------

Look at the contents of the files: `erlang_processes.[1-5]`
