
fn main() {
    fn f2(): println "one liner";

    if true!: println "if one liner";
    if false! {
        let var = "The number: ";
        let var2 = add 2., 3.;
        let concat = add "first", "second";
        let false = false!;

        print var;
        print "%i", call plus_one(2,3);
        println "%.2f", var2;
        println "one word %s", "second word";
        println "concat string: %s", concat;
        println "bool: %i", false;
    }

    println "%s %i %i %i %i", "Operations:", div(6, 3), mul(5,6), add(4,4), sub(34,35);

    call f2();
    call f2();
    call function();
}

fn plus_one(a: Int, b: Int) -> Int {
    return add add(a,b), 1;
}

fn function() {
	println "Line1";
	println "Line2";
	println "Line3";
}
