/**
 * @file
 * Initialize the Reveal handler.
 */

// More info about config & dependencies:
// - https://github.com/hakimel/reveal.js#configuration
// - https://github.com/hakimel/reveal.js#dependencies
Reveal.initialize({
		center: true,
		width: "100%",
		height: "100%",
		margin: 0,
		minScale: 1,
		maxScale: 1,
		history: true,
		hash: true,

		controls: false,
		controlsLayout: 'edges',

		// Arrow keys progress through sub-slides
		// keyboard: {
		// 	39: 'next', // right key
		// 	37: 'prev'	// left key
		// },

		pointer: {
			key: "q", // key to enable pointer, default "q", not case-sensitive
			color: "red", // color of a cursor, default "red" any valid CSS color
			pointerSize: 16, // pointer size in px, default 12
			alwaysVisible: false, // should pointer mode be always visible? default "false"
			tailLength: 10, // NOT IMPLEMENTED YET!!! how long the "tail" should be? default 10
		  },
		  
		plugins: [ RevealMarkdown, RevealHighlight, RevealNotes, RevealPointer ],
		
});