
// covSpectraJS Bryan Hanson, DePauw University, May 2015

// Functions to draw decorations
// Functions to handle selection of spectra to plot

var drawOutlines = function() {
    // Outline the areas in which we will draw things

    // svg.append('rect') // outline main window (for troubleshooting)
    // 	.attr({x: 0, y: 0,
    // 	       width:(winWidth),
    // 	       height: (winHeight),
    // 	       stroke: 'black',
    // 	       'stroke-width': 3,
    // 	       fill:'white'});

  svg.append('rect') // outline spec area
  	.attr({x: lPad,
       y: tPad,
       width: specWidth,
       height: specHeight,
       stroke: 'black',
       'stroke-width': 1.5,
       fill: 'white'});

  svg.append('rect') // outline map area
  	.attr({x: lPad + specWidth + gap,
       y: tPad + specHeight - mapHeight,
       width: mapWidth,
       height: mapHeight,
       stroke: 'black',
       'stroke-width': 1.5,
       fill:'white'});

     // create a definition containing a gradient (of sorts...)
   	var grade = svg.append("defs")
   	  .append("linearGradient")
      .attr("x1", 0)
      .attr("x2", 0)
      .attr("y1", 1)
      .attr("y2", 0)
   	  .attr("id", "KEY_GRADIENT");

   	// add a non-gradient gradient for each segment
   	keyScale.forEach(function(d, i) {
   	  grade.append("stop")
   	    .style("stop-color", d)
   	    .style("stop-opacity", 1)
   	    .attr("offset", i / (keyScale.length));
   	  grade.append("stop")
   	    .style("stop-color", d)
   	    .style("stop-opacity", 1)
   	    .attr("offset", (i + 1) / (keyScale.length));
     	});

   svg.append('rect') // outline scale key
   	.attr({x: lPad + specWidth + gap,
         y: tPad,
         width: mapWidth*0.1,
         height: (specHeight - mapHeight)*0.9,
         stroke: 'black',
         'stroke-width': 1.5,
         fill:'url(#KEY_GRADIENT)'});


} // end of drawOutlines

var drawYaxisLabel = function() {
  var labx, laby;
  labx = lPad - 80;
  laby = tPad + 0.5*specHeight;

  svg.append("text")
    .attr("x", labx)
    .attr("y", laby)
    .attr("text-anchor", "middle")
    .attr("font-family", "sans-serif")
    .style("font-size", "18px")
    .attr("transform", "rotate(-90," + labx + "," + laby + ")")
    .text(yLabel);
}


var drawKeyLabel = function() {
  var labx, laby;
  labx = lPad + specWidth + gap + mapWidth*0.1 + 80;
  laby = tPad + (specHeight - mapHeight)*0.9*0.5;

  svg.append("text")
    .attr("x", labx)
    .attr("y", laby)
    .attr("text-anchor", "middle")
    .attr("font-family", "sans-serif")
    .style("font-size", "18px")
    .attr("transform", "rotate(90," + labx + "," + laby + ")")
    .text("correlation coefficient");
}

var drawTitle = function() {
  var title
  title = d3.select("#title")
    .append("text")
    .text(Desc);
}


var setupCursorTextBox = function() {
  S1 = d3.select("#controls")
    .append("form")
    .append("label")
    .text("cursor    ")
    .insert("input")
    .attr({
        type: "text",
        id: "CURSOR_TB",
        class: "cursor",
        value: ""
    })
} // End of setupCursorTextBox
