<?php
?>
<div id="menubar">

<ul id="menutitle">
  <li id="menu1">
    <div><a href="">File</a></div>
    <ul>
      <li><a href="#">Open Database</a></li>
      <li><a href="#">Open Table</a></li>
      <li><a href="#">Open View</a></li>
      <li><a href="#">Create new table</a></li>
      <li><a href="#">Quit</a></li>
    </ul>
  </li>
  <li id="menu2">
    <div><a href="">Edit</a></div>
    <ul>
      <li><a href="#">Add Rows</a></li>
      <li><a href="#">Crunch table</a></li>
      <li><a href="#">Delete</a></li>
    </ul>
  </li>
  <li id="menu3">
    <div><a href="">View</a></div>
    <ul>
      <li><a href="#">Arrange</a></li>
      <li><a href="#">Record View</a></li>
      <li><a href="#">Sort</a></li>
      <li><a href="#">Subset</a></li>
      <li><a href="#">Group</a></li>
      <li><a href="#">Ungroup</a></li>
      <li><a href="#">Join</a></li>
      <li><a href="#">Leftjoin</a></li>
      <li><a href="#">Nojoin</a></li>
      <li><a href="#">Outerjoin</a></li>
      <li><a href="#">Theta</a></li>
      <li><a href="#">Join Keys</a></li>
      <li><a href="#">Find Forward</a></li>
      <li><a href="#">Find Backward</a></li>
      <li><a href="#">Row #</a></li>
    </ul>
  </li>
  <li id="menu4">
    <div><a href="">Options</a></div>
    <ul>
      <li><a href="#">Hide Null cols</a></li>
      <li><a href="#">Keep window</a></li>
      <li><a href="#">Max rows</a></li>
      <li><a href="#">Readable times</a></li>
    </ul>
  </li>
  <li id="menu5">
    <div><a href="#">Graphics</a></div>
    <ul>
      <li><a href="#" onClick="document.webdbe.graphics.value='map';document.webdbe.submit()">Map</a></li>
      <li><a href="#" onClick="document.webdbe.graphics.value='thmb';document.webdbe.submit()">ShowThumbnail</a></li>
      <li><a href="#" onClick="document.webdbe.graphics.value='img';document.webdbe.submit()">ShowImage</a></li>
      <li><a href="#" onClick="document.webdbe.graphics.value='time';document.webdbe.submit()">MakeTimeLapse</a></li>
      <li><a href="#" onClick="document.webdbe.graphics.value='plot';document.webdbe.submit()">PlotStation</a></li>
    </ul>
  </li>
  <li id="menu6">
    <div><a href="#">Help</a></div>
    <ul>
      <li><a href="#" onClick="window.open('webdbe_subwin.php?help=about','About webdbe','statusbar=1,menubar=1,height=300,width=600')">About webdbe</a></li>
      <li><a href="#" onClick="window.open('webdbe_subwin.php?help=schema','On Schema','statusbar=1,menubar=1,height=300,width=600')">On Schema</a></li>
      <li><a href="#" onClick="window.open('webdbe_subwin.php?help=table&mytable=<?php echo $mytable ; ?>','On <?php echo $mytable ; ?>','statusbar=1,menubar=1,height=300,width=600')">On <?php echo $mytable ; ?></a></li>
    </ul>
  </li>
</ul>

<hr/>

</div>

<div id="table">
<?php
