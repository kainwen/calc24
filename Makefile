compile: rational.erl calc24.erl
	erlc calc24.erl
	erlc rational.erl

run:
	erl -pa

clean:
	rm *beam
