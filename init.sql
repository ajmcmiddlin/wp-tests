CREATE USER 'wordpress'@'%' IDENTIFIED BY 'wordpress';
GRANT ALL PRIVILEGES ON wordpress.* TO 'wordpress' WITH GRANT OPTION;
FLUSH PRIVILEGES;

UPDATE wp_options SET option_value = 'a:1:{i:0;s:32:"basic-auth-plugin/basic-auth.php";}' where option_name = 'active_plugins';
