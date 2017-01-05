

// plotSpectraJS Bryan Hanson, DePauw University, February 2015

// Brush related functions

var activateBrush = function() {
  // Creates the brush, appends it, and defines its behavior

  var brushed, brush;

  // This helper function must remain inside the activateBrush function

  brushed = function() { // Handles the response to brushing
    var br, coords, minX, maxX, x0, xL, xU, spanX;

    br = document.getElementById("BRUSH")
    coords = d3.brushSelection(br)
    // In d3 v4 the coordinates returned are screen pixels
    minX = coords[0]
    maxX = coords[1]
    x0 = lPad + specWidth + gap // dim of map region
    x1 = x0 + mapWidth
    xL = ((minX-x0)/(x1-x0)) // as a frac of map region
    xU =  ((maxX-x0)/(x1-x0))
    spanX = Dx[1] - Dx[0]
    xD = [((spanX*xL) + Dx[0]), ((spanX*xU) + Dx[0])] // xD is global
    brushExtent = [xL, xU] // brushExtent is global
    clearSpectra();
    updateOffset()
    }   // end of brushed

    resetBrush = function() { // Handles single click in map region
      // https://github.com/d3/d3-brush/issues/10 for correct way to
      // clear a brush.  However this works.
      brushExtent = [0, 1] // brushExtent is global
      clearSpectra();
      updateOffset()
      d3.selectAll(".cursorGuide") // remove the cursor
    	    .remove();
      document.getElementById("CURSOR_TB").value = ""
      xD = Dx // resets cursor value
    }   // end of resetBrush

// The following sets up and positions the brush

  brush = d3.brushX() // This is a 1D brush
  x0 = lPad + specWidth + gap // dim of map region
  y0 = winHeight - mapHeight - tPad
  x1 = x0 + mapWidth
  y1 = winHeight - tPad
  brush.extent([[x0, y0],[x1, y1]])
    .on("end", brushed) // carry out the zoom
    .on("start", resetBrush) // single click resets view

  svg.append("svg") // Appends the svg to include the brush
  	.attr("class", "brush")
    .attr("id", "BRUSH")
    .call(brush)

} // end of activateBrush

// Guide related functions.
// Note that these need to respond to brushing, which changes xD

var activateGuides = function() {

  var getMouseX, followMouse;

  // Controls the guides (cursor) in the spec area
  // IMPORTANT: xD, yD, mX, mY, are global variables
  // Code is copied from exCon, but horiz. cursor removed here

  getMouseX = function() {

    var mouse;

  // Get the mouse coordinates & report in terms of [0...1]
  	mouse = d3.mouse(document.getElementById("SPECTRUM"));
    // mouse seems to report the whole window pixel coordinates
    // not just the spectra region
    mX = mouse[0];

  	if (mX < 0) {mX = 0}; // truncate low
  	if (mX > specWidth) {mX = specWidth}; // truncate high
  	mX = mX/specWidth // as fraction
  	followMouse();
  } // end of getMouseX

  followMouse = function() { // This draws the guides

    var xPos, vertU, vertL, vEnds, line;

    xPos = (mX * specWidth) + lPad // mX now in pixels
  	vertU = {x: xPos, y: tPad } // x, y at the top of window
    // x, y at the bottom of window:
  	vertL = {x: xPos, y: tPad + specHeight }
  	vEnds = [vertU, vertL];

  	d3.selectAll(".cursorGuide") // remove previous lines
  	    .remove();

  	line = d3.line()
  	    .x(function(d) { return d.x;})
  	    .y(function(d) { return d.y;})

  	svg.append("path")
      	    .attr("class", "line")
            .attr("class", "cursorGuide")
      	    .attr("d", line(vEnds))

    if (mX <= 0.01 || mX >= 0.99) {
      d3.selectAll(".cursorGuide").remove();
      document.getElementById("CURSOR_TB").value = ""
    }

  } // end of followMouse

    getMouseX(); // This starts it all off
    setupCursorReporting()

} // end of activateGuides

// This function reports the cursor coordinates in native unit
// to a form/text box

var setupCursorReporting = function() {
  var xNat
  xNat = xD[0] + ((mX) * (xD[1] - xD[0]))

  if ((xUnit = "wavenumber") || (xUnit = "Wavenumber")) {
    document.getElementById("CURSOR_TB").value = xNat.toFixed(1)
  }

  if (xUnit = "ppm") {
    document.getElementById("CURSOR_TB").value = xNat.toFixed(3)
  }

} // End of setupCursorReporting
