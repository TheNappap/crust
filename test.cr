struct Data {
    a: Int,
    b: Int,
    arr: [Int;2],
}

impl Data {
    fn new() -> Data {
        return new Data{a: 88, b: 99, arr: [65,66]}
    }

    fn a(self) -> Int {
        return self.a;
    }
}

trait Num {
    fn num() -> Int;
    fn double_num(self) -> Int {
        return self.num() * self.num();
    }
}

impl Num for Data {
    fn num(self) -> Int {
        return self.a + self.b;
    }
}

enum Option {
    None,
    Some,
}

impl Option {
    fn some_func(self) -> String {
        return "ENUM METHOD!";
    }
}

fn print_bool(bool: Bool) -> String {
    if bool {
        return "true"
    } else {
        return "false"
    }
}

fn main() {
    call basics(); 
    call loops_and_arrays();
    call groups();
    call custom_data(new Data{a: 15, b: 41, arr: [2,3]});
    call impl_blocks();
    call pattern_matching();
}

fn pattern_matching() {
    println "Pattern matching:";
    let opt = new Option::Some;
    match opt {
        case Option::Some: println "matched Some!";
        case Option::None: println "matched None!";
        case _: println "default case!";
    }
    println;
}

fn impl_blocks() {
    println "Impl blocks:";
    println "full fn method a(): %i", call Data::a(call Data::new());
    println "method call a(): %i", call Data::new().a();
    println "static fn data member b: %i", call Data::new().b;
    println "What is this? %s", new Option::Some.some_func();
    println "trait method call num(): %i", call Data::new().num();
    println "trait method call double_num(): %i", call Data::new().double_num();
    println;
}

fn custom_data(data: Data) {
    println "Custom data:";

    mut data.b = 333;
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

    println;
}

fn groups() {
    println "Groups:";
    {
        let group = {
            let a = 8;
            let b = a + 5;
        }
        println "group result: %i", group;
    }
    println;
}

fn loops_and_arrays() {
    println "Loops and arrays:";

    let add = 0;
    while add != 100 {
        println "%i %s", add, call print_bool(add == 50);
        mut add = 25 + add;
    }

    let arr = [[2,3],[4,5],[6,7]];
    //mut index arr[1] = 12; //TODO
    println "array[2][1]: %i", arr[2][1];

    // TODO <, <=, >, >= operators
    iter 0..3 => for i: if i == 1 {
        iter arr => for i {
            println "iter array %i %i", i;
        }
    }

    iter 3..7 => map x: forward x+2
                map y: forward y*2
                filter z: forward z==14
                for w {
                    println "transformed range %i", w;
                }

    println;
}

fn basics() {
    println "Basics:";

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
        println "bool: %s", call print_bool(!false);
    } else {
        println "else was activated";
    }
    println "%s %i %i %i %i %i", "Operations:", 6/3, 5*6, 4+4, 34-35, 5*-4 - --9/3;

    println;
}

fn plus_one(a: Int, b: Int) -> Int {
    return a + b + 1;
}
