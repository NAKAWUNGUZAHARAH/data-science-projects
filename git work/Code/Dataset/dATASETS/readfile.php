<?php
	$filename = "pr_1901_2016_ARG.csv";
	$fp = fopen($filename, "r");

	$content = fread($fp,filesize($filename));
	$lines = explode("\n", $content);
	fclose($fp);
	
	$organisedTable='Year,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec'; 
	$x=0;
	foreach($lines as $line){  
		echo $line."<br>"; 
		$temp = explode(",", $line); 
		if($x%12==0){ $organisedTable .="\n";  $organisedTable .= $temp[1].",";  }else{  $organisedTable .=",";  } 
		$organisedTable .= $temp[0]; 
		$x++;
	} 
	$fpp = fopen("tmp1.csv", "w");
	fwrite($fpp, $organisedTable); 
	fclose($fpp);
?>
	
	
	
	
	
