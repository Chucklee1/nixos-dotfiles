{inputs, ...}: {
  home = [
    ({pkgs, ...}: {
      # external dependancies
      home.packages = [
        inputs.ratune.packages.${pkgs.stdenv.hostPlatform.system}.default
      ];
      home.file.".config/ratune/config.toml".source = (pkgs.formats.toml {}).generate "ratune-config" {
        server = {
          url = "https://navidrome.chucklee.uk";
          username = "goat";
          # super duper safe way to get password
          # if you get in however, I will have your device hostname and ip address...
          password_command = "sops --decrypt $HOME/Repos/nixos-dotfiles/secrets.yaml | yq -r .navidrome.goat";
        };

        player.default_volume = 70;
        player.max_bit_rate = 0;
        player.daemon = true;

        theme.foreground = "#d8dee9"; # primary text
        theme.dimmed = "#60728A"; # muted / secondary text
        theme.preset = "terminal"; # static | dynamic (default) | terminal | os

        ui.browsetab.mode = "artists";
        ui.album_art_backend = "kitty-apc"; # default: ratatui-image
        ui.general.tab_bar_position = "bottom";
        ui.row_now_playing = {
          bar_height = 4;
          layout = "row";
          box_location = "right";
          show_controls = true;
          show_progress = true;
          box_include_controls = false;
          box_include_progress = false;
          progress_style = "██░";
        };
        ui.hometab.recent_albums.show_art = true;
        ui.hometab.layout.top_height_percent = 50;
        # Pick any three of: recent_albums, recent_tracks, rediscover, recently_added, recently_released
        ui.hometab.layout.panels = ["recent_albums" "recent_tracks" "rediscover"];


        ui.nptab.queue.position = "right";
        ui.nptab.art.show = true;
        ui.nptab.art.position = "left";
        ui.nptab.lyrics_pane = {
          enabled = true;
          visible = false;
          location = "right";
        };
        ui.nptab.visualizer_pane = {
          enabled = true;
          visible = true;
          location = "right";
        };

        library.enabled = true;
        library.fzf.binary = "fzf";

        cache.enabled = false;
        cache.max_size_gb = 2; # in GB

        lyrics.source = ["lrclib" "subsonic" "netease"];
        lyrics.lrclib_url = "https://lrclib.net";
        lyrics.cache_enabled = false;
      };
    })
  ];
}
