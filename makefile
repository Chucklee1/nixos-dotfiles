BUILD = nixos-rebuild switch 
CLEAN = nix-collect-garbage
EXTRAFLAGS = --show-trace --impure --flake

laptop:
	$(BUILD) $(FLAGS) .\#laptop

desktop: 
	$(BUILD) $(FLAGS) .\#desktop

clean:
	$(CLEAN)

full-clean:
	$(CLEAN) -d

optimise:
	nix store optimise
