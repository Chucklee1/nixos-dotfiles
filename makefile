FLAGS = --show-trace --impure --flake

laptop:
	nixos-rebuild switch $(FLAGS) .\#laptop

desktop: 
	nixos-rebuild switch $(FLAGS) .\#desktop

clean:
	nixos-collect-garbage

fullclean:
	nixos-collect-garbage -d
