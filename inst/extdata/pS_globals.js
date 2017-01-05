
// plotSpectraJS Bryan Hanson, DePauw University, February 2015

// Global variables collected here, with NO exceptions
// These are in addition to the global variables passed from R

// Define layout variables for each subsection of the display
// These are fixed sizes, they don't change even if the window is resized
// You need to use pretty much all the screen anyway, to see details.

// Official partial abbrevs to be used:
// locator map: map, spectrum area: spec
// The entire area containing all of the above is the window,
// use win (the div is 'main')

var winWidth = 0.95*screen.width, // define sizes of windows
    winHeight = 0.7*screen.height, // these values determine the aspect ratio of the layout
    specWidth = 0.77*winWidth, // < 90% total to allow for padding and gaps
    specHeight = 0.95*winHeight,
    mapWidth = 0.2*winWidth
    mapHeight = 0.18*winHeight;

var gap = 0.02*winWidth, // gap for aesthetics
    lPad = (winWidth - specWidth - gap - mapWidth)/2,
    // this approach ensures centering in main window
    // rPad = lPad,
    tPad = (winHeight - specHeight)/2
    // bPad = tPad;

// Define the master global variable for drawing purposes.
// Everything gets appended to this object

var svg = d3.select('#main')
// The full window is appended to #main and named 'svg'.
// This simply defines a variable/window into which we
// can draw, nothing is drawn.
    .append('svg')
    .attr("width", window.innerWidth)
    .attr("height", window.innerHeight)

// Initialize xD & yD
// Dx & Dy are the data domains passed in by the R function
// Domains are in native units, as supplied by the user

// xD & yD are the currently displayed spectral limits in native units

var xD = Dx,
    yD = Dy;

// Define refRow, the reference spectrum to be drawn in the map area

var refRow = 1;

// Initialize an array with 2 elements which will hold the
// brush extent in fractional units

var brushExtent = [0, 1]

// Initialize the value of offset.  This is needed globally so that
// if updateOffset was not called via the slider, a value is available.

var offset = 0.0

// Initialize the mouse X position, mX

var mX = 0.0
