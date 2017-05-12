<?php

require_once 'messages.php';

//site specific configuration declartion
define('BASE_PATH', 'http://cfans-mysql01.oit.umn.edu/');
define('DB_HOST', 'http://cfans-mysql01.oit.umn.edu/');
define('DB_USERNAME', 'ansci_mootalk');
define('DB_PASSWORD', 'v2PSvqctbNAR3PK8');
define('DB_NAME', 'ansci_mootalk');

//Facebook App Details
define('FB_APP_ID', '497426220463682');
define('FB_APP_SECRET', '23ba72fe5a99a2e01661aa6693e49bd4');
define('FB_REDIRECT_URI', 'http://mootalk.umn.edu/login.php');

//Google App Details
define('GOOGLE_APP_NAME', 'apprumen');
define('GOOGLE_OAUTH_CLIENT_ID', '295944032706-2mk7h807vk9hoa22g5jdgh7s5smsi95a.apps.googleusercontent.com');
define('GOOGLE_OAUTH_CLIENT_SECRET', 'enLSQMQiKkiA6jwGU9ihrLWE');
define('GOOGLE_OAUTH_REDIRECT_URI', 'http://localhost/login.php');
define("GOOGLE_SITE_NAME", 'http://mootalk.umn.edu/login.php');

//Twitter login
define('TWITTER_CONSUMER_KEY', 'YOUR_CONSUMER_KEY');
define('TWITTER_CONSUMER_SECRET', 'YOUR_CONSUMER_SECRET');
define('TWITTER_OAUTH_CALLBACK', 'YOUR_OAUTH_CALLBACK');

function __autoload($class) {
    $parts = explode('_', $class);
    $path = implode(DIRECTORY_SEPARATOR, $parts);
    require_once $path . '.php';
}