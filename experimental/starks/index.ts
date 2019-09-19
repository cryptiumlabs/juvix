import { Stark } from '@guildofweavers/genstark';

// define a STARK for this computation
const fooStark = new Stark(`
define Foo over prime field (2^32 - 3 * 2^25 + 1) {

    // define transition function
    transition 1 register in 64 steps {
        out: $r0 + 2;
    }

    // define transition constraints
    enforce 1 constraint {
        out: $n0 - ($r0 + 2);
    }
}`);

// create a proof that if we start computation at 1, we end up at 127 after 64 steps
const assertions = [
    { register: 0, step: 0,  value: 1n   },  // value at first step is 1
    { register: 0, step: 63, value: 127n }   // value at last step is 127
];
const proof = fooStark.prove(assertions, [1n]);

// verify that if we start at 1 and run the computation for 64 steps, we get 127
const result = fooStark.verify(assertions, proof);
console.log(result); // true
