<?php // index.php

include_once 'header.php';

if ($_GET['unit_level'] == 'region' OR $_GET['unit_level'] == 'ateneo') {
  $lat = getlat($db, $_GET["unit"]);
  $lon = getlon($db, $_GET["unit"]);
} else {
  $lat = getlat($db, $_GET["ateneo"]);
  $lon = getlon($db, $_GET["ateneo"]);
}

if (strpos($_GET["unit"], 'wd') !== false) {
  $unit_name = getUnitName($db, $_GET["unit"]);
} else {
  $unit_name = $_GET["unit"];
}

if (strpos($_GET["unit"], 'wd') !== false) {
  $unit_title = "<a href = 'https://www.wikidata.org/wiki/" .
  str_replace('wd:', '', $_GET["unit"]) .
    "' target='_blank'>" .
    $unit_name . "</a>";
} else {
  $unit_title = "<a href = 'https://www.wikidata.org/wiki/" .
  str_replace('wd:', '', $_GET["ateneo"]) .
    "' target='_blank'>" .
    $unit_name . " (". getUnitName($db, $_GET["ateneo"]).")</a>";
}

echo '<div id="container">
        <div id="header">
        <h1>', $unit_title, '</h1>
    </div>
    
    <div id="leftbar">
        <h2>Shortcuts</h2>
    </div>';

echo "<div id='content'>";
returnUnitQueryResults($db, $_GET["unit"]);
echo "</div>";

echo "<div id='rightbar'>";

echo "<div id = 'unit-summary-stats'>";
echo "<p><b>Unit:</b> " . $unit_title . "</p>";
echo "<p><b>Staff:</b> " . getNumberOfDocenti($db, $_GET['unit'], $_GET['unit_level'], $_GET['ateneo']) . "</p>";

if ($_GET['unit_level'] != 'region') {
  echo "<p><b>Region:</b> " . getRegion($db, $_GET['ateneo']) . "</p>";
}

echo "</div>";

echo "<div id = 'unit-prob-plot'>";
echo '<iframe width="240px" height="100px" src="http://146.118.107.12:3838/sim_dist/">';
echo "<p>Loading...</p>";
echo "</iframe>";
echo "  </div>";

echo "<div id = 'unit-on-map'>";

echo <<<EOT
      <script defer="defer" type="text/javascript">
        var map = new OpenLayers.Map('unit-on-map');
        var wms = new OpenLayers.Layer.WMS( "OpenLayers WMS",
            "http://vmap0.tiles.osgeo.org/wms/vmap0", {layers: 'basic'} );
        map.addLayer(wms);
        map.zoomToMaxExtent();
        map.setCenter(new OpenLayers.LonLat({$lon}, {$lat}), 7);
        var vectorLayer = new OpenLayers.Layer.Vector("Overlay");
        var feature = new OpenLayers.Feature.Vector(
        new OpenLayers.Geometry.Point({$lon}, {$lat}),
	{some:'data'},
	{externalGraphic: 'img/marker.png', graphicHeight: 21, graphicWidth: 16});
         vectorLayer.addFeatures(feature);
         map.addLayer(vectorLayer);
      </script>
EOT;

echo "</div>
  
    </div>";
  
include_once 'footer.php';

?>