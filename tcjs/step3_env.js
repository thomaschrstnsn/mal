var reader = require('./reader.js'),
    read_str = reader.read_str;

var types = require('./types');

function READ(a) {
    return read_str(a);
}

function eval_ast(ast, env) {
    if (types.isSymbol(ast)) {
        var symbol = types.nameOf(ast);
        return env.get(symbol);
    }
    if (types.isList(ast)) {
        var res = ast.map(function (x) {
            return EVAL(x, env);
        });
        types.toList(res);
        return res;
    }
    return ast;
}

function define_form(ast, env) {
    if (ast.length !== 3) {
        throw new Error('expected 3 element in define form');
    }

    if (!types.isSymbol(ast[1])) {
        throw new Error('expected symbol as key in define form');
    }

    var key = types.nameOf(ast[1]);
    var val = EVAL(ast[2], env);
    env.set(key, val);

    return null;
}

function EVAL(ast, env) {
    if (types.isList(ast)) {
        var first = ast[0];
        if (types.isSymbol(first)) {
            switch (types.nameOf(first)) {
            case 'def!': return define_form(ast, env);
            case 'let*': return let_form(ast, env);
            }
        }

        var evaled = eval_ast(ast, env);
        var func = evaled.shift();
        return func.apply(undefined, evaled);
    }
    return eval_ast(ast, env);
}

var pr_str = require('./printer.js').pr_str;

function PRINT(a) {
    return pr_str(a);
}

var Env = require('./env').Env;

var floorIt = Math.floor;

var repl_env = Env(null);
repl_env.set('+', function (a, b) { return floorIt(a + b);});
repl_env.set('-', function (a, b) { return floorIt(a - b);});
repl_env.set('*', function (a, b) { return floorIt(a * b);});
repl_env.set('/', function (a, b) { return floorIt(a / b);});

function rep(a) {
    return PRINT(EVAL(READ(a), repl_env));
}

function DEBUG_rep(a) {
    console.log('env: ', repl_env.keys());

    var r = READ(a);
    console.log("read:  ", r);

    var e = EVAL(r, repl_env);
    console.log("eval:  ", e);

    var p = PRINT(e);
    console.log("print: ", p);

    return p;
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
