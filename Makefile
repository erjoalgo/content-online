SHELL=/bin/bash

all: bin/content-online-service

bin/content-online-service:
	mkdir -p bin
	buildapp --eval '(declaim (optimize (speed 3)))' --eval '(load #p"~/quicklisp/setup.lisp")' --eval '(load #P"lisp/youtube-comments.asd")' --eval "(ql:quickload 'youtube-comments)" --eval "(disable-debugger)" --entry YOUTUBE-COMMENTS:main --output bin/content-online-service

clean:
	rm -r bin

install: bin/content-online-service
	cp -f $< /usr/local/bin/
