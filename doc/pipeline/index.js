const StateMachine = require('javascript-state-machine');
const visualize = require('javascript-state-machine/lib/visualize');

const fsm = new StateMachine({
  init: 'frontend',
  transitions: [
    { name: 'desugar'         , from: 'frontend'    , to: 'core+datpat' },
    { name: 'typecheck'       , from: 'core+datpat' , to: 'core+datpat' },
    { name: 'reify-datpat'    , from: 'core+datpat' , to: 'core' },
    { name: 'typecheck'       , from: 'core'        , to: 'core' },
    { name: 'erase'           , from: 'core'        , to: 'erasedcore' },
    { name: 'optimise'        , from: 'erasedcore'  , to: 'optcore' },
    { name: 'target-r1cs'     , from: 'optcore'     , to: 'r1cs' },
    { name: 'target-michelson', from: 'optcore'     , to: 'michelson' },
    { name: 'target-inet'     , from: 'optcore'     , to: 'inetir' },
    { name: 'inet-to-llvm'    , from: 'inetir'      , to: 'llvm' }
  ]
});

console.log(visualize(fsm, {name: 'Compiler Pipeline', orientation: 'vertical' }));
