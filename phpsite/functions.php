<?php // functions.php

function getlat($db, $unit) {
  $sql = 'SELECT lat FROM (SELECT wikidata_id AS id, lat, lon FROM regione_wt_coord UNION SELECT wikidata_id AS id, lat, lon FROM ateneo) AS reg_ateneo WHERE id = ?';
  $stmt = $db->prepare($sql);
  $stmt->bindParam(1, $unit, PDO::PARAM_STR);
  $stmt->execute();
  $result = $stmt->fetch(PDO::FETCH_ASSOC);
  return $result['lat'];
}

function getlon($db, $unit) {
  $sql = 'SELECT lon FROM (SELECT wikidata_id AS id, lat, lon FROM regione_wt_coord UNION SELECT wikidata_id AS id, lat, lon FROM ateneo) AS reg_ateneo WHERE id = ?';
  $stmt = $db->prepare($sql);
  $stmt->bindParam(1, $unit, PDO::PARAM_STR);
  $stmt->execute();
  $result = $stmt->fetch(PDO::FETCH_ASSOC);
  return $result['lon'];
}

function getRegion($db, $ateneo) {
  $sql = 'SELECT nome FROM ateneo JOIN regione ON ateneo.regione_id = regione.wikidata_id WHERE ateneo.wikidata_id = ?';
  $stmt = $db->prepare($sql);
  $stmt->bindParam(1, $ateneo, PDO::PARAM_STR);
  $stmt->execute();
  $result = $stmt->fetch(PDO::FETCH_ASSOC);
  return $result['nome'];
}

function getNumberOfDocenti($db, $unit, $unit_level, $ateneo = NULL, $year = "2016") {
  
  if ($unit_level == 'region') {
    $sql = 'SELECT docenti FROM count_docenti_by_year_region WHERE regione_id = ? AND anno = ?';
    $stmt = $db->prepare($sql);
    $stmt->bindParam(1, $unit, PDO::PARAM_STR);
    $stmt->bindParam(2, $year, PDO::PARAM_STR);
    $stmt->execute();
    $result = $stmt->fetch(PDO::FETCH_ASSOC);
    return $result['docenti'];
  } elseif ($unit_level == 'ateneo') {
    $sql = 'SELECT docenti FROM count_docenti_by_year_ateneo WHERE ateneo_id = ? AND anno = ?';
    $stmt = $db->prepare($sql);
    $stmt->bindParam(1, $unit, PDO::PARAM_STR);
    $stmt->bindParam(2, $year, PDO::PARAM_STR);
    $stmt->execute();
    $result = $stmt->fetch(PDO::FETCH_ASSOC);
    return $result['docenti'];
  } elseif ($unit_level == 'facolta') {
    $sql = 'SELECT docenti FROM count_docenti_by_year_facolta WHERE facolta = ? AND ateneo_id = ? AND anno = ?';
    $stmt = $db->prepare($sql);
    $stmt->bindParam(1, $unit, PDO::PARAM_STR);
    $stmt->bindParam(2, $ateneo, PDO::PARAM_STR);
    $stmt->bindParam(3, $year, PDO::PARAM_STR);
    $stmt->execute();
    $result = $stmt->fetch(PDO::FETCH_ASSOC);
    return $result['docenti'];
  }  elseif ($unit_level == 'dipartimento') {
      $sql = 'SELECT docenti FROM count_docenti_by_year_dipartimento WHERE dipartimento = ? AND ateneo_id = ? AND anno = ?';
      $stmt = $db->prepare($sql);
      $stmt->bindParam(1, $unit, PDO::PARAM_STR);
      $stmt->bindParam(2, $ateneo, PDO::PARAM_STR);
      $stmt->bindParam(3, $year, PDO::PARAM_STR);
      $stmt->execute();
      $result = $stmt->fetch(PDO::FETCH_ASSOC);
      return $result['docenti'];
    }  
}

function getUnitName($db, $unit) {

  $sql = 'SELECT name FROM ((SELECT wikidata_id AS id, wikidata_label AS name FROM ateneo) UNION (SELECT wikidata_id AS id, nome AS name FROM regione)) AS alias WHERE id = ?';
  $stmt = $db->prepare($sql);
  $stmt->bindParam(1, $unit, PDO::PARAM_STR);
  $stmt->execute();
  $result = $stmt->fetch(PDO::FETCH_ASSOC);

  return $result['name'];
  
}

function returnUnitQueryResults($db, $unit, $year = "2016") {

  $sql = 'SELECT * FROM simulation_long_df WHERE unit = ? AND year = ?';
  $stmt = $db->prepare($sql);
  $stmt->bindParam(1, $unit, PDO::PARAM_STR);
  $stmt->bindParam(2, $year, PDO::PARAM_STR);
  $stmt->execute();
  $result = $stmt->fetchAll(PDO::FETCH_ASSOC);

  echo "<div id = 'unit-query-results'>";
  echo '<table id="example" class="display" cellspacing="0" width="90%" style = "font-size: 80%">';

  echo "<thead></tr>";
  echo "<th>sim_id</th>";
  echo "<th>Surname</th>";
  echo '<th><a href="#" data-toggle="tooltip" title="Number of people employed in the region, university, faculty or department with this surname and as percentage of total number of employees.">Staff with this surname</a></th>';

  echo '<th><a href="#" data-toggle="tooltip" title="Estimate of people with this surname living in the region based on data collected from the telephone register and as percentage of the total population.">Population with this surname</a></th>';

  echo '<th><a href="#" data-toggle="tooltip" title="Percentage of simulations that returned equal or higher number of employees with the same surname.">Probability of observing by chance</a></th>';

  echo "</tr></thead>";

  echo "<tbody>";
  foreach($result as $row) {
    echo "<tr>";
    echo "<td>", $row['sim_id'], "</td>";
    echo "<td>", $row['surname'], "</td>";
    echo "<td>", $row['docenti_wt_surname']," (",
      round($row['docenti_wt_surname'] / $row['unitpop'] * 100, 4),
      "%)</td>";
    echo "<td>", round(($row['hh_mean_size'] * $row['hh_wt_surname']) /
		       $row['perc_hh_wt_fixline'], 0), " (",
      round(($row['hh_mean_size'] * $row['hh_wt_surname']) /
		       ($row['largepop'] * $row['perc_hh_wt_fixline']) *100, 4), "%)</td>";
    echo "<td>", round($row['p_of_observing']*100, 4), "%</td>";
    echo "</tr>";
  }
  echo "</tbody>";
  echo "</table>";
  echo "</div>";
  
}

?>