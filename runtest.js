// Load the Wasm file
const fs = require('fs');
const wasmFile = './conqiler/wat.wasm';
//const wasmFile = './program.wasm';
const buffer = fs.readFileSync(wasmFile);

// Create imports object with implementation for `malloc`
let wasmInstance;
let last = 0;
const imports = {
    env: {
        // Implementation for `malloc`
        malloc: size => {
            const memory = wasmInstance.exports.memory;
            const currentMemory = last;
            const memoryStart = currentMemory + 8;
            const memoryEnd = memoryStart + Number(size);

            if (memoryEnd > memory.buffer.byteLength) {
                throw new Error('Out of memory');
            }

            last = memoryEnd;
            return memoryStart;
        }
    }
};

// Instantiate the Wasm module with the provided imports
WebAssembly.instantiate(buffer, imports).then(result => {
    wasmInstance = result.instance;

    // Call a function from the Wasm module
    const wasmFunctionResult = wasmInstance.exports._start(200);
    console.log(wasmInstance.exports.memory.buffer)

    // Do something with the result
    console.log(wasmFunctionResult);
});