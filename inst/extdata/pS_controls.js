
// plotSpectraJS Bryan Hanson, DePauw University, February 2015

// Functions to draw decorations
// Functions to handle selection of spectra to plot

var drawOutlines = function() {
    // Outline the areas in which we will draw things

    // If uncommented, need to use attr not attrs

    // svg.append('rect') // outline main window (for troubleshooting)
    // 	.attrs({x: 0, y: 0,
    // 	       width:(winWidth),
    // 	       height: (winHeight),
    // 	       stroke: 'black',
    // 	       'stroke-width': 3,
    // 	       fill:'white'});

    svg.append('rect') // outline spec area
      .attr("x", lPad)
      .attr("y", tPad)
      .attr("width", specWidth)
      .attr("height", specHeight)
      .attr("stroke", "black")
      .attr("stroke-width", 1.5)
      .attr("fill", "white")

    svg.append('rect') // outline map area
       .attr("x", lPad + specWidth + gap)
       .attr("y", tPad + specHeight - mapHeight)
       .attr("width", mapWidth)
       .attr("height", mapHeight)
       .attr("stroke", "black")
       .attr("stroke-width", 1.5)
       .attr("fill", "white")
} // end of drawOutlines

var drawTitle = function() {
  var title
  title = d3.select("#title")
    .append("text")
    .text(Desc);
}

var setupSelections = function() {
  var form, labels, all, i;

  form = d3.select("#sample_names")
    .append("form");

  // Draw the labels
  labels = form.selectAll("label")
    .data(Names)
    .enter()
    .append("label")
    .text(function(d) {return d;})
    .insert("input")
    .attr("type", "checkbox")
    .attr("id", "CB_LABELS")
    .attr("class", "checkbox")
    .attr("name", function(d, i) {return i;})
    .attr("value", function(d, i) {return i;})
    .property("checked", function(d, i) {
      if (i == 0) return 1
      if (i > 0) return 0
    });

  // Add event listeners
  all = document.getElementsByClassName('checkbox');
  for (i = 0; i < all.length; i++) {
    all[i].addEventListener("change", updateSampleBOOL)
  }
} // End of setupSelections

var updateSampleBOOL = function() {
  var all, i;

  all = document.getElementsByClassName('checkbox');
  for (i = 0; i < all.length; i++) {
    if (all[i].checked) {
      // console.log("Sample", all[i].name, "was selected")
      if (sampleBOOL[i] == 0) sampleBOOL[i] = 1
      }
      if (!all[i].checked) {
        if (sampleBOOL[i] == 1) sampleBOOL[i] = 0
        }
    }

  clearSpectra()
  updateOffset() // Eventually draws the spectra
} // End of updateSampleBOOL

var setupCursorTextBox = function() {
  S1 = d3.select("#controls")
    .append("form")
    .append("label")
    .text("cursor    ")
    .insert("input")
    .attr("type", "text")
    .attr("id", "CURSOR_TB")
    .attr("class", "cursor")
    .attr("value", "")
} // End of setupCursorTextBox

var setupSliders = function() {
  var S1;

  S1 = d3.select("#controls")
    .append("form")
    .append("label")
    .text("offset    ")
    .insert("input")
    .attr("type", "range")
    .attr("id", "OFFSET_SLIDER")
    .attr("class", "slider")
    .attr("min", 0.0)
    .attr("max", 100.0)
    .attr("value", 0.0)

    // Add event listeners

    d3.select("#OFFSET_SLIDER").on("mouseup", function() {
      // Update the global variable offset
      // Avoids problems when there are combinations of brushing
      // and selections
      offset = this.value; // update global value in case updateOffset
                           // called w/o a value
      updateOffset(this.value);
      });

} // End of setupSliders


var updateOffset = function(value) {
  var dmin, dmax, val, i, j, rows;

  if (!value) value = offset // Get stored global value if
  // not provided via slider (either at launch or certain
  // function calls come w/o value for value)

  // Convert value to be on the scale of the original data

  dmin = arrayMinMax(D0)[0]
  dmax = arrayMinMax(D0)[1]
  drange = dmax - dmin
  val = value*1.2*drange/100

  // Iterate over the elements of D0, adding the value*i to each element,
  // store the result in global D1, which is the data actually plotted.
  // Do this only for the rows that will be plotted, otherwise there
  // is an unsightly gap where a row was not selected.

  rows = bool2indices()
  for (i = 0; i < rows.length; i++) {
    for (j = 0; j < D1[0].length; j++) {
      D1[rows[i]][j] = D0[rows[i]][j] + val * i;
    }
  }

  clearSpectra()
  drawSpectra(D1)
} // End of updateOffset
