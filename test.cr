
fn main() {
    fn f2(): println "one liner";
    
    call f2();

    if true$: println "if one liner"
    else: println "else one liner";
    if false$ {
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

    call loops_and_arrays();
}

fn loops_and_arrays() {
    println "loops_and_arrays:";

    let add = 0;
    while add != 100 {
        println "%i %i", add, add == 50;
        mut add = 25 + add;
    }

    let arr = array[5,6,7];
    println "array: %i, %i, %i", arr;
    iter array[6,2,4] {} for i {
        println "iter %i", i;
        let add = 0;
    }
}

fn plus_one(a: Int, b: Int) -> Int {
    return a + b + 1;
}
