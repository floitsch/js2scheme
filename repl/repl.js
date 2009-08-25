function repl() {
    while (true) {
	display("> ");
	var __stmt = readLine();
	while (true) {
	    try {
		print(eval(__stmt));
		break;
	    } catch(e) {
		if (e instanceof SyntaxError &&
		    e.message == "unexpected end of file: EOF")
		    __stmt += "\n" + readLine();
		    else {
			print(e);
			break;
		    }
	    }
	}
    }
}
function main(args) {
    repl();
}
