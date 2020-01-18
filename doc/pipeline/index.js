const StateMachine = require('javascript-state-machine');
const visualize = require('javascript-state-machine/lib/visualize');

const fsm = new StateMachine({
  init: 'frontend',
  transitions: [
    { name: 'desugar' , from: 'frontend'  , to: 'core' },
    { name: 'erase'   , from: 'core'      , to: 'erasedcore' },
    { name: 'optimise', from: 'erasedcore', to: 'optcore' },
    { name: 'target'  , from: 'optcore'   , to: 'machine' }
  ]
});

console.log(visualize(fsm, {name: 'Compiler Pipeline', orientation: 'horizontal' }));
