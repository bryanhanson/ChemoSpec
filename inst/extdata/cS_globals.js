
// covSpectraJS Bryan Hanson, DePauw University, May 2015

// Global variables collected here, with NO exceptions
// These are in addition to the global variables passed from R

// Define layout variables for each subsection of the display
// These are fixed sizes, they don't change even if the window is resized
// 15" MacBookPro Retina screen is 1440 x 900

// Official partial abbrevs to be used:
// locator map: map, spectrum area: spec
// The entire area containing all of the above is the window,
// use win (the div is 'main')

var winWidth = 0.85*screen.width, // define sizes of windows
    winHeight = 0.7*screen.height, // these values determine the aspect ratio of the layout
    specWidth = 0.77*winWidth, // < 90% to allow for padding and gaps
    specHeight = 0.95*winHeight,
    mapWidth = 0.2*winWidth
    mapHeight = 0.18*winHeight;

var gap = 0.02*winWidth, // gap for aesthetics
    lPad = (screen.width - specWidth - gap - mapWidth)/2,
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
// Dx & Dy are the data ranges passed in by the R function
// Domains are in native units, as supplied by the user

// xD & yD are the current data ranges (zoomed by brushing)

var xD = Dx,
    yD = Dy;

// Initialize an array with 2 elements which will hold the
// brush extent in fractional units

var brushExtent = [0, 1]

// Initialize the mouse X position, mX

var mX = 0.0
