<?php // init_db.php

$dbhost = '999.999.999.99'; 
$dbname = 'db_name'; 
$dbuser = 'user';
$dbpass = 'password';

$db = new PDO("mysql:host=$dbhost;dbname=$dbname;charset=utf8", $dbuser, $dbpass);
// set the PDO error mode to exception
$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
  
?>