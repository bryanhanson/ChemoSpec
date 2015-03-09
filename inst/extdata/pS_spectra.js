

// plotSpectraJS Bryan Hanson, DePauw University, February 2015

// Spectrum drawing functions.
// Two tasks handled here:
// 1.  Draw the reference spectrum in the context region.  This is fixed.
// 2.  Draw varying number of spectra in the spec region;
// these must respond to brushing, which changes xD,
// as well as changes in the sliders.

// Some helper functions

var arraySize = function(array) { // merged from several SO post ideas
	// js is row-major
	// assumes 2D array
	// assumes each row has the same length (= no. of columns)
	var nrows, ncols;

	nrows = array.length
	ncols = array[0].length // length of first row
	return [nrows, ncols]
}


var arrayMinMax = function(array) { // 2D array only
  // Based on http://stackoverflow.com/a/23397365/633251
	// Initialize with the value in the first row
	var amax, amin, i, aRow, array;

	amax = Math.max.apply(Math, array[0]);
	amin = Math.min.apply(Math, array[0]);
	for (i = 1; i < array.length; i++){
	    aRow = array[i];
	    if (amax < Math.max.apply(Math, aRow)) {
	        amax = Math.max.apply(Math, aRow);
	      }
	    if (amin > Math.min.apply(Math, aRow)) {
	        amin = Math.min.apply(Math, aRow);
	      }
	  }
	return [amin, amax]
}

var bool2indices = function() {
	// Convert a boolean array into the corresponding indices
	// If true, the index is kept
	var rows, i;

	rows = []
	for (i = 0; i < sampleBOOL.length; i++ ){
			if (sampleBOOL[i] == 1) rows.push(i);
		}
	return rows;
}

// End of helper functions

var getSpectrumLimits = function() {

	// This function gets the left & right edge indices
	// starting from native units (ie Dx, Dx units) ????
	// and taking brushing into account.
	// js indices start at zero!
	var nc, left, right;

	nc = arraySize(D0)[1];
	left = Math.floor(brushExtent[0]*nc);
	right = Math.floor(brushExtent[1]*nc);
	if (right > nc) right = nc
	return [left, right];
} // end of getSpectrumLimits


var getSpectrumFreqValues = function() {

	// Get the spectrum freq values (x dimension)
	// taking into account any brushing.

  var lIndex, rIndex, xdata;

	lIndex = getSpectrumLimits()[0];
	rIndex = getSpectrumLimits()[1];
	xdata = Freq.slice(lIndex, rIndex);
	return xdata;

} // end of getSpectrumFreqValues


var getSpectrumIntValues = function(data) {

	// Get the spectrum intensity values (y dimension)
	// taking into account any brushing.
	// sampleBOOL determines which spectra are actually drawn

  var rows, lIndex, rIndex, crow, ybase, ydata, yvals, i, data;

  rows = bool2indices()
	lIndex = getSpectrumLimits()[0];
	rIndex = getSpectrumLimits()[1];

	yvals = []; // start empty, add each element one at a time
	for (i = 0; i < rows.length; i++ ) {
		crow = rows[i]
		ybase = data[crow]
		ydata = ybase.slice(lIndex, rIndex)
		yvals.push(ydata);
	}

  return yvals;
} // end of getSpectrumIntvalues


var drawRefSpec = function(row) {

	// Draw a reference spectrum in the map region.
	// The x and y coords are fixed.

  var xdata, ydata, xy, i, xscl, yscl, spectrum, refSpec;

	xdata = Freq;
	ydata = D0[row];
	xy = []; // start empty, add each element one at a time
	for (i = 0; i < xdata.length; i++ ) {
	    xy.push({x: xdata[i], y: ydata[i]});
	}

	xscl = d3.scale.linear()
    .domain(d3.extent(xy, function(d) {return d.x;})) //use just the x part
    .range([0, mapWidth])

	yscl = d3.scale.linear()
		.domain(d3.extent(xy, function(d) {return d.y;})) //use just the y part
    .range([mapHeight-5, 5]) // keeps line from touching outline

	spectrum = d3.svg.line()
    .x(function(d) { return xscl(d.x);}) // apply the x scale to the x data
    .y(function(d) { return yscl(d.y);}) // apply the y scale to the y data

	refSpec = svg.append("g")

	refSpec.append("path")
		.attr("transform", "translate(" + (lPad + specWidth + gap) +","
			+ (tPad + specHeight - mapHeight) + ")")
		.attr({width: mapWidth,
			height: mapHeight,
			"class": "line",
			"class": "refSpectrum",
			"d": spectrum(xy) // use the return value of spectrum(xy) as 'd'
			})

	drawRefAxis(xscl)

} // end of drawRefSpec


var drawSpectra = function(data) {

	// Draw one or more spectra in the spec region

	var xdata, ydata, data, xscl, yscl, rowData, xy, i, j, spectrum, Spectra, rows;

	xdata = getSpectrumFreqValues()
	ydata = getSpectrumIntValues(data)
	// ydata is an array with just the rows of interest

	xscl = d3.scale.linear()
    .domain(d3.extent(xdata))
    .range([0, specWidth])
	yscl = d3.scale.linear()
		.domain(arrayMinMax(ydata))
    .range([specHeight-5, 5]) // keeps line from touching outline

	// master loop to draw one spectrum at a time

  rows = bool2indices()

	for (i = 0; i < ydata.length; i++ ) { // i is the spectrum counter
		rowData = ydata[i]
		xy = []; // start empty, add each element one at a time
		for (j = 0; j < xdata.length; j++ ) {
		    xy.push({x: xdata[j], y: rowData[j]});
		}

		spectrum = d3.svg.line()
	    .x(function(d) { return xscl(d.x);}) // apply the x scale to the x data
	    .y(function(d) { return yscl(d.y);}) // apply the y scale to the y data

    // id's assigned via code should be upper case
		Spectra = svg.append("g")
			.attr({
				"id": "SPECTRUM"
				})

		Spectra.append("path")
			.attr("transform", "translate(" + lPad +","
				+ (tPad) + ")")
			.attr({width: specWidth,
				height: specHeight,
				stroke: Colors[rows[i]],
				"class": "line",
				"class": "spectrum",
				"d": spectrum(xy)}) // use the return value of spectrum(xy) as 'd'
  } // End of master loop

	drawSpecAxis(xscl)

} // end of drawSpectra


var drawSpecAxis = function(xScale) {
	var xAxis;

  d3.select("#SPEC_AXIS").remove(); // remove existing axis
  xAxis = d3.svg.axis()
  	.scale(xScale)
  	.orient("bottom");

	svg.append("g")
		.attr("id", "SPEC_AXIS")
		.attr("class", "axis")
		// the math portion below must be in parens
		// the math is evaluated and '+' concatenates the strings
		.attr("transform", "translate(" + lPad +","
			+ (tPad + specHeight + 10) + ")")
		.call(xAxis);
}


var drawRefAxis = function(xScale) {
	var xAxis;

  xAxis = d3.svg.axis()
  	.scale(xScale)
  	.orient("bottom")
		.ticks(5);

	svg.append("g")
		.attr("class", "axis")
		// the math portion below must be in parens
		// the math is evaluated and '+' concatenates the strings
		.attr("transform", "translate(" + (lPad + specWidth + gap) +","
			+ (tPad + specHeight + 10) + ")")
		.call(xAxis);
}


var clearSpectra = function() {
    d3.selectAll(".spectrum").remove();
}
