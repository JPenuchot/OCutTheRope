<?php
/*
 * OCutTheRope Server
 *
 *   get.php
 *   Allow user to get a list of levels and to get a level
 *
 * by Théophile Walter
 */

// We don't display HTML but text
header("Content-Type: text/plain");

// Print an error message and exit
function error($message) {
	echo "error:$message";
	exit;
}

if (isset($_GET['getlist'])) {

	// Return the list of available levels
	$scan = scandir("levels");

	foreach ($scan as $level) {
	 	if (substr($level, -4) == ".lvl") {
	 		$id = substr($level, 0, -4);
	 		echo "$id\n";
	 		echo file_get_contents("levels/".$id.".desc")."\n";
	 	}
	 }

} else if (isset($_GET['getlevel'])) {

	// Check if the level exists
	if (!ctype_alnum($_GET['getlevel']) || !file_exists("levels/".$_GET['getlevel'].".lvl")) {
		error("This level does not exists!");
	}

	// Display the level
	readfile("levels/".$_GET['getlevel'].".lvl");



} else {

	error("You must specify the GET parameters \"getlist\" or \"getlevel\"!");

}