all:
	(cd src;$(MAKE))
	(cd deps/erlydtl/src;$(MAKE))
	(cd deps/mochiweb/src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	(cd deps/erlydtl/src;$(MAKE) clean)
	(cd deps/mochiweb/src;$(MAKE) clean)
