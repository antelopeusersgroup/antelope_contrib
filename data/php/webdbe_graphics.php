<?php
// Get the form variables
$dbname = $_GET['dbname'];
$mytable = $_GET['mytable'];
$record = $_GET['record'];
?>
<html>
<head>
<title>Webdbe Child Window - Graphics Page</title>
<link rel="stylesheet" href="webdbe_style.css" type="text/css" />
<script language="javascript">
<!--
function winSize() {
	window.resizeTo( 800,600 )
}
//-->
</script>

</head>
<body onLoad="winSize();">
<table align="center" frame="border" rules="groups|cols" class="main">
<thead>
	<tr>
		<td class="tablename"><?php echo "$dbname.$mytable"; ?></td>
	</tr>
	<tr>
		<td class="menu">Record Number: <?php echo "$record"; ?></td>
	</tr>
</thead>
<tbody>
	<tr>
		<td align="center">Insert graphic here</td>
	</tr>
	<tr>
		<td align="center"><a href="javascript:void(0)" onClick="self.close()">Close window</a></td>
	</tr>
</tbody>
</table>
</body>
</html>
