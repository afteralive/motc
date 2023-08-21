.PHONY: clean

motc: motc.rkt
	raco exe motc.rkt

clean:
	rm motc