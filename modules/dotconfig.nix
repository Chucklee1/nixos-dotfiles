{
  home.global = [
    {
      home.file = {
        ".latexmkrc".text =
          #sh
          ''
            add_cus_dep( 'acn', 'acr', 0, 'makeglossaries' );
            add_cus_dep( 'glo', 'gls', 0, 'makeglossaries' );
            $clean_ext .= " acr acn alg glo gls glg";
            sub makeglossaries {
              my ($base_name, $path) = fileparse( $_[0] );
              my @args = ( "-d", $path, $base_name );
              if ($silent) { unshift @args, "-q"; }
              return system "makeglossaries", @args;
            }
          '';
        # issue with nix shell
        ".config/nixpkgs/config.nix".text =
          # nix
          ''
            { nixpkgs.config.allowUnfree = true; }
          '';
      };
    }
  ];
}
