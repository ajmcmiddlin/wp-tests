{
network.description = "Wordpress";

wordpress =
  { config, pkgs, ... }:
  {
    services.mysql = {
      enable = true;
      package = pkgs.mysql;
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
              }
            ];
        }
      ];
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
