function READ(a) {
    return a;
}

function EVAL(a) {
    return a;
}

function PRINT(a) {
    console.log(a);
    return a;
}

function rep(a) {
    return PRINT(EVAL(READ(a)));
}

var readline = require('./node_readline');
var pr_str = require('./printer.js').pr_str;

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
