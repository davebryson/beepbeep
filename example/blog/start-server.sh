#!/bin/sh
cd `dirname $0`

exec erl -pa $PWD/ebin $PWD/../../deps/*/ebin $PWD/../../ebin -boot start_sasl -s reloader -s blog_example
