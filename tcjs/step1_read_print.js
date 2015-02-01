var read_str = require('./reader.js').read_str;

function READ(a) {
    return read_str(a);
}

function EVAL(a) {
    return a;
}

var pr_str = require('./printer.js').pr_str;

function PRINT(a) {
    return pr_str(a);
}

function DEBUG_rep(a) {
    // return PRINT(EVAL(READ(a)));
    var r = READ(a);
    console.log("read:  ", r);

    var e = EVAL(r);
    console.log("eval:  ", e);

    var p = PRINT(e);
    console.log("print: ", p);

    return p;
}

function rep(a) {
    return PRINT(EVAL(READ(a)));
}

var readline = require('readline'),
    rl = readline.createInterface(process.stdin, process.stdout);

rl.setPrompt('mal> ');
rl.prompt();

rl.on('line', function(line) {
    var r = rep(line);
    console.log(r);
    rl.prompt();
}).on('close', function() {
    console.log('Have a great day!');
    process.exit(0);
});
