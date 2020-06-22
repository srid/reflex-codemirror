codemirror-example:
	cd example && nix-build -A ghcjs.example -o result-reflex-codemirror
	$(BROWSER) ./example/result-reflex-codemirror/bin/reflex-codemirror-exe.jsexe/index.html
