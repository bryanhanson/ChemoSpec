

// covSpectraJS Bryan Hanson, DePauw University, May 2015

// Spectrum drawing functions.
// Two tasks handled here:
// 1.  Draw the reference spectrum in the context region.  This is fixed.
// 2.  Draw the spectra in the spec region;
// this must respond to brushing, which changes xD

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

// End of helper functions


var getSpectrumLimits = function() {

	// This function gets the left & right edge indices
	// using the brush extent
	// js indices start at zero!

	var nc, left, right;

	nc = Freq.length;
	left = Math.floor(brushExtent[0]*nc);
	right = Math.ceil(brushExtent[1]*nc);
	if (right > nc) right = nc;
	right = right + 1;  // For consistency with array.slice indexing

	// console.log("Freq has ", nc, "data points")
	// console.log("left is", left)
	// console.log("right is", right)
	// console.log("")

	return [left, right];

} // end of getSpectrumLimits


var trimColors = function() {

	// Trim the list of colors in myc much as we do the frequencies
	// taking into account any brushing.

  var lIndex, rIndex, tcol;

	lIndex = getSpectrumLimits()[0];
	rIndex = getSpectrumLimits()[1];
	tcol = myc.slice(lIndex, rIndex);
	return tcol;

} // end of getSpectrumFreqValues


var getSpectrumFreqValues = function() {

	// Get the spectrum freq values (x dimension)
	// taking into account any brushing.

  var lIndex, rIndex, xdata;

	lIndex = getSpectrumLimits()[0];
	rIndex = getSpectrumLimits()[1];
	xdata = Freq.slice(lIndex, rIndex);
	return xdata;

} // end of getSpectrumFreqValues


var getSpectrumIntValues = function() {

	// Get the spectrum intensity values (y dimension)
	// taking into account any brushing.

  var lIndex, rIndex, yvals;

	lIndex = getSpectrumLimits()[0];
	rIndex = getSpectrumLimits()[1];
  yvals = Y.slice(lIndex, rIndex)
  return yvals;
} // end of getSpectrumIntvalues


var drawRefSpec = function() {

	// Draw a reference spectrum in the map region.
	// The x and y coords are fixed, not affected by brushing

  var xdata, ydata, xy, i, xscl, yscl, spectrum, refSpec, grade;

	xdata = Freq;
	ydata = Y;

	xy = []; // start empty, add each element one at a time
	for (i = 0; i < xdata.length; i++ ) {
	    xy.push({x: xdata[i], y: ydata[i]});
	}

	// create a definition containing a gradient (of sorts...)
	var grade = svg.append("defs")
	  .append("linearGradient")
	  .attr("id", "REF_SPEC_GRADIENT");

	// add a non-gradient gradient for each segment
	myc.forEach(function(d, i) {
	  grade.append("stop")
	    .style("stop-color", d)
	    .style("stop-opacity", 1)
	    .attr("offset", i / (myc.length));
	  grade.append("stop")
	    .style("stop-color", d)
	    .style("stop-opacity", 1)
	    .attr("offset", (i + 1) / (myc.length));
	});

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
		.attr({
			"id": "REF_SPECTRUM"
			})

	refSpec.append("path")
		.attr("transform", "translate(" + (lPad + specWidth + gap) +","
			+ (tPad + specHeight - mapHeight) + ")")
		.attr({
			width: mapWidth,
			height: mapHeight,
			stroke: "url(#REF_SPEC_GRADIENT)",
			"class": "line",
			"class": "refSpectrum",
			"d": spectrum(xy) // use the return value of spectrum(xy) as 'd'
			})

	drawRefAxis(xscl)

} // end of drawRefSpec


var drawSpectrum = function() {

	// Draw the spectrum in the spec region

	var xdata, ydata, xscl, yscl, xy, i, spectrum, Spectra, grade, yaxlab;

	xdata = getSpectrumFreqValues();
	ydata = getSpectrumIntValues();
  tmyc = trimColors();

	// console.log("There are:", tmyc.length, "colors")
	// console.log("The first 10 are:", tmyc.slice(0,10))

	xy = []; // start empty, add each element one at a time
	for (i = 0; i < xdata.length; i++ ) {
	  xy.push({x: xdata[i], y: ydata[i] });
	}

	// create a definition containing a gradient (of sorts...)
	var grade = svg.append("defs")
	  .append("linearGradient")
	  .attr("id", "SPEC_GRADIENT");

	// add a non-gradient gradient for each segment
	tmyc.forEach(function(d, i) {
	  grade.append("stop")
	    .style("stop-color", d)
	    .style("stop-opacity", 1)
	    .attr("offset", i / (tmyc.length));
	  grade.append("stop")
	    .style("stop-color", d)
	    .style("stop-opacity", 1)
	    .attr("offset", (i + 1) / (tmyc.length));
	});

	xscl = d3.scale.linear()
    .domain(d3.extent(xdata))
    .range([0, specWidth])
	yscl = d3.scale.linear()
		.domain(d3.extent(ydata))
    .range([specHeight-5, 5]) // keeps line from touching outline

	spectrum = d3.svg.line()
    .x(function(d) { return xscl(d.x);}) // apply the x scale to the x data
    .y(function(d) { return yscl(d.y);}) // apply the y scale to the y data

	Spectra = svg.append("g")
		.attr({
			"id": "SPECTRUM" // id's assigned via code should be upper case
			})

	Spectra.append("path")
		.attr("transform", "translate(" + lPad +","
			+ (tPad) + ")")
		.attr({
			width: specWidth,
			height: specHeight,
			stroke: "url(#SPEC_GRADIENT)",
			"class": "line",
			"class": "spectrum",
			"d": spectrum(xy)  // use the return value of spectrum(xy) as 'd'
			})

	drawSpecXaxis(xscl)
	drawSpecYaxis(yscl)

} // end of drawSpectrum



var drawSpecXaxis = function(xScale) {
	var xAxis;

  d3.select("#SPEC_X_AXIS").remove(); // remove existing axis

  xAxis = d3.svg.axis()
  	.scale(xScale)
  	.orient("bottom");

	svg.append("g")
		.attr("id", "SPEC_X_AXIS")
		.attr("class", "axis")
		// the math portion below must be in parens
		// the math is evaluated and '+' concatenates the strings
		.attr("transform", "translate(" + lPad +","
			+ (tPad + specHeight + 10) + ")")
		.call(xAxis);
}


var drawSpecYaxis = function(yScale) {
	var yAxis;

  d3.select("#SPEC_Y_AXIS").remove(); // remove existing axis

  yAxis = d3.svg.axis()
  	.scale(yScale)
		.tickFormat(d3.format("s"))
  	.orient("left");

	svg.append("g")
		.attr("id", "SPEC_Y_AXIS")
		.attr("class", "axis")
		// the math portion below must be in parens
		// the math is evaluated and '+' concatenates the strings
		.attr("transform", "translate(" + (lPad - 10) +","
			+ (tPad) + ")")
		.call(yAxis);
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

var drawKeyAxis = function() {
	var sAxis, scl;

	scl = d3.scale.linear()
		.domain([1, -1])
    .range([0, (specHeight - mapHeight)*0.9])
  sAxis = d3.svg.axis()
  	.scale(scl)
  	.orient("right")
		.ticks(10);

	svg.append("g")
		.attr("class", "axis")
		// the math portion below must be in parens
		// the math is evaluated and '+' concatenates the strings
		.attr("transform", "translate(" + (lPad + specWidth + gap + mapWidth*0.1 + 10) +","
			+ (tPad) + ")")
		.call(sAxis);
}

var clearSpectrum = function() {
    d3.selectAll(".spectrum").remove();
		d3.select("#SPEC_GRADIENT").remove();
		drawSpectrum();
		markDriver();
}

var markDriver = function() { // Marks the driver peak

	var xdata, xPos, vertU, vertL, vEnds, line;

	d3.selectAll(".driverPeak") // remove previous lines
			.remove();

	xdata = getSpectrumFreqValues();

	if ((driver > xD[0] && (driver < xD[1]))) {

		xPos = (driver - xD[0])/(xD[1] - xD[0])
		xPos = xPos * specWidth + lPad
		vertU = {x: xPos, y: tPad } // x, y at the top of window
		vertL = {x: xPos, y: tPad + specHeight } // x, y at the bottom of window
		vEnds = [vertU, vertL];

		line = d3.svg.line()
				.x(function(d) { return d.x;})
				.y(function(d) { return d.y;})

		svg.append("path")
						.attr("class", "line")
						.attr("class", "driverPeak")
						.attr("d", line(vEnds))
						.attr("stroke-dasharray", "5,5")
						.attr("stroke", "gray")
  }
} // end of markDriver
