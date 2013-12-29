you-me-irc-rkt
==============

A WIP toy project: implement an IRC bot in Racket.

I'm almost entirely new to Racket, so this code is probably not even correct, let alone idiomatic or good.

### Status

It connects to the server, sends PASS, NICK, USER, and JOIN. (It doesn't even check for a successful connection, mind you.)

It echoes what it gets from the server.

I got stuck more than a few times, so many thanks to https://github.com/schuster/racket-irc for inspiration here and there. 

### TODO

*   Clean up rampant FIXMEs and TODOs.
*   Handle PING/PONG.
*   Check for successful connect.
*   Parse commands -- at least PRIVMSG commands and such.
*   Add generalized "event" handler, to add more features later.
*   Use async channels, custodians, places, whatever?

