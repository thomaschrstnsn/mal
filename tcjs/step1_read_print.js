var reader = require('./reader.js'),
    read_str = reader.read_str;

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

var readline = require('./node_readline.js');

if (process.argv[2] === 'debug') {
    console.log("debugging REPL");
    rep = DEBUG_rep;
}

// repl loop
if (typeof require !== 'undefined' && require.main === module) {
    // Synchronous node.js commandline mode
    while (true) {
        var line = readline.readline("mal-user> ");
        if (line === null) { break; }
        try {
            if (line) { console.log(rep(line)); }
        } catch (exc) {
            if (exc instanceof reader.BlankException) { continue; }
            if (exc.stack) { console.log(exc.stack); }
            else           { console.log(exc); }
        }
    }
}
