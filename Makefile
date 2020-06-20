example:
	nix-build example.nix -A ghcjs.reflex-codemirror -o result-reflex-codemirror
	$(BROWSER) ./result-reflex-codemirror/bin/reflex-codemirror-exe.jsexe/index.html
