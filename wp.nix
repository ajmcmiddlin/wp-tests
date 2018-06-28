{

network.description = "Wordpress";

wordpress =
  { config, pkgs, ... }:
  let
    basicAuthPlugin = pkgs.stdenv.mkDerivation {
      name = "basic-auth-plugin";
      # Download the theme from the wordpress site
      src = pkgs.fetchurl {
        url = https://github.com/WP-API/Basic-Auth/archive/9e9d5267c7805c024f141d115b224cdee5a10008.zip;
        sha256 = "b7f4fe0e6064040eeec2d34c27296cc69c92ed015c8d4164cf86af002fde2ddd";
      };
      # We need unzip to build this package
      buildInputs = [ pkgs.unzip ];
      # Installing simply means copying all files to the output directory
      installPhase = "mkdir -p $out; cp -R * $out/";
    };

    twentySeventeen = pkgs.stdenv.mkDerivation {
      name = "theme-twenty-seventeen";
      # Download the theme from the wordpress site
      src = pkgs.fetchurl {
        url = https://downloads.wordpress.org/theme/twentyseventeen.1.6.zip;
        sha256 = "0cch9bvap4r0775f055mynbf0d6k8zrqyn2mdwkbn6rr12hn526b";
      };
      # We need unzip to build this package
      buildInputs = [ pkgs.unzip ];
      # Installing simply means copying all files to the output directory
      installPhase = "mkdir -p $out; cp -R * $out/";
    };
  in
  {
    services.mysql = {
      enable = true;
      package = pkgs.mysql;
      initialScript = ./init.sql;
    };

    services.httpd = {
      enable = true;
      logPerVirtualHost = true;
      adminAddr="andrew@qfpl.io";
      extraModules = [
        { name = "php7"; path = "${pkgs.php}/modules/libphp7.so"; }
      ];

      virtualHosts = [
        {
          hostName = "wordpress";
          extraSubservices =
            [
              {
                serviceType = "wordpress";
                dbPassword = "wordpress";
                wordpressUploads = "/data/uploads";
                languages = [ "en_GB" ];
                plugins = [ basicAuthPlugin ];
                themes = [ twentySeventeen ];
              }
            ];
        }
      ];
    };

    # HTTP, HTTPS, MySQL
    networking.firewall.allowedTCPPorts = [ 80 443 3306 ];
  };
}
