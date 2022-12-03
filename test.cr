struct Data {
    a: Int,
    b: Int,
    arr: [Int;2],
}

enum Option {
    None,
    Some,
}

fn main() {
    call basics(); 
    call loops_and_arrays();
    call groups();
    call custom_data(new Data{a: 15, b: 41, arr: [2,3]});
}

fn custom_data(data: Data) {
    //mut data.a = 666; //TODO
    println "custom data member a: %i", data.a;
    println "custom data member b: %i", data.b;
    println "custom data member arr: [%i, %i]", data.arr;

    let variant = new Option::None;
    println "enum variant None: %i", variant;
    mut variant = new Option::Some;
    println "enum variant Some: %i", variant;
    if variant == new Option::Some {
        println "if enum variant Some: %i", variant;
    }
}

fn groups() {
    {
        let group = {
            let a = 8;
            let b = a + 5;
        }
        println "group result: %i", group;
    }
}

fn basics() {
    fn f2(): println "one liner";
    
    call! f2();

    if true!: println "if one liner"
    else: println "else one liner";
    if true! {
        let var = "The number: ";
        let var2 = 2.+3.;
        let concat = add "first", "second";
        let false = false!;

        print var;
        print "%i", call plus_one(2,3);
        println "%.2f", var2;
        println "one word %s", "second word";
        println "concat string: %s", concat;
        println "bool: %i", !false;
    } else {
        println "else was activated";
    }
    println "%s %i %i %i %i %i", "Operations:", 6/3, 5*6, 4+4, 34-35, 5*-4 - --9/3;
}

fn plus_one(a: Int, b: Int) -> Int {
    return a + b + 1;
}

fn loops_and_arrays() {
    println "loops_and_arrays:";

    let add = 0;
    while add != 100 {
        println "%i %i", add, add == 50;
        mut add = 25 + add;
    }

    let arr = [[2,3],[4,5],[6,7]];
    //mut index arr[1] = 12; //TODO
    println "array[2][1]: %i", arr[2][1];
    iter arr {} for i {
        println "iter %i %i", i;
    }
    //iter 0..10 {} //TODO
}
