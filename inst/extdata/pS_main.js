
// plotSpectraJS Bryan Hanson, DePauw University, February 2015

// These function calls return nothing: they draw something, or define behaviors

// On launch, draw the initial view
// All of these cause things to be appended to global variable 'svg'
// defined in pS_globals.js
// Most access global variables

drawOutlines(); // Appends rectangles outlining the areas we'll be using.
drawTitle() // Draws text items, like title
setupSelections() // Draws checkboxes to select data & sets up event listeners
setupSliders() // Draw sliders for adjusting things
setupCursorTextBox() // Draws text box to report cursor position
drawRefSpec(refRow) // Draws the reference spectrum
updateOffset() // Draws one or more spectra (Sample no. 1 @ launch) via
               // via drawSpectra()

// Set up behaviors
// These depend on xD & yD

activateBrush(); // Handles all the brush tasks
svg.on('mousemove', activateGuides); // Handles vertical cursor
