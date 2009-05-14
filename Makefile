all:
	(cd src;$(MAKE))
	(cd deps/erlydtl/src;$(MAKE))
	(cd deps/mochiweb/src;$(MAKE))
	(cd example/blog;$(MAKE))

docs:
	erl -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application "'BeepBeep'" '"."' '[no_packages]'

clean:
	(cd src;$(MAKE) clean)
	(cd deps/erlydtl/src;$(MAKE) clean)
	(cd deps/mochiweb/src;$(MAKE) clean)
	(cd example/blog;$(MAKE) clean)
