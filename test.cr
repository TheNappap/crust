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
        "true"
    } else {
        "false"
    }
}

fn main() {
    .basics(); 
    .loops_and_arrays();
    .groups();
    .custom_data(new Data{a: 15, b: 41, arr: [2,3]});
    .impl_blocks();
    .pattern_matching();
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
    println "full fn method a(): %i", .Data::a(.Data::new());
    println "method call a(): %i", .Data::new().a();
    println "static fn data member b: %i", .Data::new().b;

    let option = new Option::Some;
    println "What is this? %s", option.some_func();

    let data = .Data::new();
    println "trait method call num(): %i", data.num();
    println "trait method call double_num(): %i", data.double_num();
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
            b
        }
        println "group result: %i", group;
    }
    println;
}

fn loops_and_arrays() {
    println "Loops and arrays:";

    let add = 0;
    while add < 100 {
        println "%i %s", add, .print_bool(add == 50);
        mut add = 25 + add;
    }

    let arr = [[2,3],[4,5],[6,7]];
    //mut index arr[1] = 12; //TODO
    println "array[2][1]: %i", arr[2][1];
    //println "Data[0]: %i", new Data::new().arr[0]; //TODO

    iter 0..3 => for i: if i == 1 {
        iter arr => for i {
            println "iter array %i %i", i;
        }
    }

    let folded_range = iter 3..7
                map x: x+2
                map y: y*2
                filter z: z<14
                fold 0, acc, w {
                    println "transformed range %i", w;
                    acc+w
                }
    println "folded range %i", folded_range;

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
        let concat = "first" + "second";
        let false = false!;

        print var;
        print "%i", .plus_one(2,3);
        println "%.2f", var2;
        println "one word %s", "second word";
        println "concat string: %s", concat;
        println "bool: %s", .print_bool(!false);
    } else {
        return;
    }
    println "%s %i %i %i %i %i", "Operations:", 6/3, 5*6, 4+4, 34-35, 5*-4 - --9/3;

    println;
}

fn plus_one(a: Int, b: Int) -> Int {
    a + b + 1
}
