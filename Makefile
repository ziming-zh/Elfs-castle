all: clean
	elm make src/Main.elm --output main.js
	mkdir build
	cp -f main.js build/main.js
	elm make src/Main.elm

	
dev:
	elm make src/Main.elm --output build/main.js
clean:
	rm -rf build/
