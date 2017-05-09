<?php
/*
 * OCutTheRope Server
 *
 *   upload.php
 *   Allow user to upload a level
 *
 * by Théophile Walter
 */

// Print an error message and exit
function error($message) {
	echo "error:$message";
	exit;
}

// Check the parameters
if (!isset($_GET['level']) || !isset($_GET['level'])) {
	error("You must specify the GET parameters \"level\" and \"description\"!");
}

// Check the input
if ($_GET['level'] === "" || $_GET['description'] === "") {
	error("Empty parameter(s)!");
}

// Saves the level
$md5 = md5($_GET['level']);
file_put_contents("levels/$md5.lvl", $_GET['level']);
file_put_contents("levels/$md5.desc", $_GET['description']);

// Print the level ID
echo $md5;
