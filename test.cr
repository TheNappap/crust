
fn main() {
    fn f2(): println "one liner";

    let var = "The number: ";
    let var2 = add 2., 3.;
    let concat = add "first", "second";

    print var;
    print "%i", call one();
    println "%.2f", var2;
    println "one word %s", "second word";
    println "concat string: %s", concat;

    call f2();
    call f2();
    call function();
}

fn one() -> Int {
    return 1;
}

fn function() {
	println "Line1";
	println "Line2";
	println "Line3";
}
