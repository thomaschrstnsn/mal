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

var readline = require('readline'),
    rl = readline.createInterface(process.stdin, process.stdout);

rl.setPrompt('mal> ');
rl.prompt();

rl.on('line', function(line) {
    rep(line);
    rl.prompt();
}).on('close', function() {
    console.log('Have a great day!');
    process.exit(0);
});
