BUILD = sudo nixos-rebuild switch 
CLEAN = sudo nix-collect-garbage
FLAGS = --show-trace --impure --flake 

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
