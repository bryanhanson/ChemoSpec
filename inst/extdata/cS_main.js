
// covSpectraJS Bryan Hanson, DePauw University, May 2015

// These function calls return nothing: they draw something, or define behaviors

// On launch, draw the initial view
// All of these cause things to be appended to global variable 'svg'
// defined in cS_globals.js
// Most access global variables

drawOutlines(); // Appends rectangles outlining the areas we'll be using.
drawTitle() // Writes description of data set at top of page
setupCursorTextBox() // Draws text box to report cursor position
drawYaxisLabel() // Draws, wait for it, the y axis label

drawKeyAxis() // Draw axis by correlation key (key is drawn by drawOutlines)
drawKeyLabel()

markDriver() // Draws a dotted vertical line at the driver position

drawRefSpec() // Draws the reference spectrum
drawSpectrum() // Draws the spectrum

// Set up behaviors
// These depend on xD

activateBrush(); // Handles all the brush tasks
svg.on('mousemove', activateGuides); // Handles vertical cursor
