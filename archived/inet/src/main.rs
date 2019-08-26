mod net;

fn print_and_reduce(n: &mut net::Net) {
    println!("Reducing net: {:?}", n);
    let steps: u32 = net::reduce_all(n);
    println!(
        "Reduced in {} steps, result: {:?}, node count: {}",
        steps,
        n,
        n.nodes.len()
    );
}

fn main() {
    println!("trivial commute 1");
    print_and_reduce(&mut net::trivial_commute_1());
    println!("trivial commute 2");
    print_and_reduce(&mut net::trivial_commute_2());
    println!("trivial commute 3");
    print_and_reduce(&mut net::trivial_commute_3());
    println!("trivial annihilate 1");
    print_and_reduce(&mut net::trivial_annihilate_1());
    println!("trivial annihilate 2");
    print_and_reduce(&mut net::trivial_annihilate_2());
    println!("test annihilate 2");
    print_and_reduce(&mut net::test_annihilate_2());
    println!("trivial annihilate 3");
    print_and_reduce(&mut net::trivial_annihilate_3());
    println!("nonterminating");
    print_and_reduce(&mut net::nonterminating());
}
