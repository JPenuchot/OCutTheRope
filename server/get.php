<?php
/*
 * OCutTheRope Server
 *
 *   get.php
 *   Allow user to get a list of levels and to get a level
 *
 * by Théophile Walter
 */

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
	 		echo file_get_contents("levels/".$id.".desc")."\n\n";
	 	}
	 }

} else {

	error();

}