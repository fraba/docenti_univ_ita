<?php // index.php

include_once 'header.php';

echo '<div id="container">
        <div id="header">
        <h1>Surname distribution in Italian universities</h1>
	<p>Click on one of the three tabs to start (<b>regions</b>, <b>universities</b> or <b>faculties</b>)</p>
	<p>Note: since the server is in Australia, maps can be slow to load. But they will eventually load.</p>
    </div>';

echo "<div id = 'unit-prob-plot'>";
echo '<iframe id = "shiny-iframe" width="1000
px" height="600px" src="http://146.118.107.12:3838/maps/">';
echo "</iframe>";
echo "  </div>";

include_once 'footer.php';

?>