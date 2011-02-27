# Readme : Contents
This is just a small repo where is play with erlang ;) I used Rebar to work with application and mochiweb mochijson2 module for working with json.
It's my first erlang project.

# TODO
1. Added better error handling
2. Build client module
3. Write better tests for application
4. Refactor init module to reflect better OTP patterns
5. Move blocking accept to other module with own nonblocking start / spawn
6. Add spawn_links to client workers.
7. Write some docs
8. Add DETS server with disk copy of inmemory data, just to simulate some safe :D
9. Refactor application to be able to work with multiple key-val storages ( change api, error , message handling )

# Build

1. compile `rebar compile`
2. compile and run tests `rebar compile eunit`

# Possible solution to think over

Ways of handling multiple storages:

a) add call that creates new directory `{ create, storage_name } -> ... `

b) add them on the fly if someone requests `{ set , storage_name, key, val } -> ...`

## storage
different types of tables set, bag, ordered_set

# Usage
compile and run it using ebin/run.sh or in erlang shell `application:start(boobshake).` then open telnet 127.0.0.1 3666 and start json talk
eg.
`{"peek":"name"}`         -> ["error","No such value"]
`{"set":{"name":"kuba"}}` -> ["ok","value added"]
`{"peek":"name"}`         -> ["ok", "kuba"]

## ekhm?!
Application now can only "set" value and "peek"

# Thanks

Thanks to mochiweb team for json module :). Thanks guys, just borrowed it :> and Spec'u for saying i should aim more for Ocaml then Erlang and whole Static vs Dynamic typing conversation.