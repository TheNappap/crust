
fn main() {
    fn f2(): println "one liner";

    if true$: println "if one liner"
    else: println "else one liner";
    if true$ {
        let var = "The number: ";
        let var2 = 2.+3.;
        let concat = add "first", "second";
        let false = false$;

        print var;
        print "%i", call plus_one(2,3);
        println "%.2f", var2;
        println "one word %s", "second word";
        println "concat string: %s", concat;
        println "bool: %i", !false;
    } else {
        println "%s %i %i %i %i %i", "Operations:", 6/3, 5*6, 4+4, 34-35, 5*-4 - --9/3;
    }

    let add = 0;
    while add != 100 {
        println "%i %i", add, add == 50;
        mut add = 25 + add;
    }

    let arr = array[5,6,7];
    println "array: %i, %i, %i", arr;
    iter array[6,2,4];
    iter arr;

    call f2();
    call f2();
    call function();
}

fn plus_one(a: Int, b: Int) -> Int {
    return a + b + 1;
}

fn function() {
	println "Line1";
	println "Line2";
	println "Line3";
}
