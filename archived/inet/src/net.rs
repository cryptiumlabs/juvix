#[derive(Clone, Debug)]
pub struct Net {
    pub nodes: Vec<Option<Node>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Kind {
    CON,
    DUP,
    ERA,
    WIRE,
    LOOP,
}

#[derive(Clone, Debug)]
pub struct Node {
    pub kind: Kind,
    pub ports: Vec<Port>,
}

#[derive(Clone, Debug)]
pub enum Port {
    Pointer { index: u32, slot: u32 },
    Free { name: String },
}

pub fn new_node(net: &mut Net, kind: Kind, ports: Vec<Port>) -> u32 {
    let node: u32 = net.nodes.len() as u32;
    net.nodes.push(Some(Node {
        kind: kind,
        ports: ports,
    }));
    return node;
}

pub fn remove_node(net: &mut Net, index: u32) {
    net.nodes[index as usize] = None;
}

pub fn set_ports(net: &mut Net, index: u32, ports: Vec<Port>) {
    match net.nodes[index as usize].as_mut() {
        Some(node) => {
            node.ports = ports;
        }
        None => {}
    }
}

pub fn relink(net: &mut Net, node: Node, old_port: u32, new: u32, new_port: u32) {
    match node.ports[old_port as usize] {
        Port::Pointer { index, slot } => {
            if let Some(counter) = &net.nodes[index as usize].clone() {
                let mut new_node = counter.clone();
                new_node.ports[slot as usize] = Port::Pointer {
                    index: new,
                    slot: new_port,
                };
                net.nodes[index as usize] = Some(new_node);
            }
        }
        _ => {}
    }
}

pub fn empty3() -> Vec<Port> {
    return vec![
        Port::Free {
            name: String::from("x"),
        },
        Port::Free {
            name: String::from("y"),
        },
        Port::Free {
            name: String::from("z"),
        },
    ];
}

pub fn cleanup(net: &mut Net) {
    let mut index: u32 = 0;
    while index < net.nodes.len() as u32 {
        match net.nodes[index as usize] {
            Some(_) => {
                index += 1;
            }
            None => {
                net.nodes.remove(index as usize);
            }
        }
    }
}

pub fn reduce(net: &mut Net) -> bool {
    let mut next: u32 = 0;
    let count: u32 = net.nodes.len() as u32;
    while next < count {
        if let Some(node) = net.nodes[next as usize].clone() {
            if node.kind == Kind::WIRE || node.kind == Kind::LOOP {
                // TODO: reduce wires which connect nodes?
                next = next + 1;
                continue;
            }
            let primary: &Port = &node.ports[0];
            if let Port::Pointer { index, slot } = primary {
                if *slot == 0 {
                    let counter_index = *index;
                    let counter: Node = net.nodes[counter_index as usize].clone().unwrap();
                    match node.kind {
                        Kind::CON => {
                            // commute 1 (c-d)
                            if counter.kind == Kind::DUP {
                                let n1: u32 = new_node(net, Kind::CON, empty3());
                                let n2: u32 = new_node(net, Kind::CON, empty3());
                                let n3: u32 = new_node(net, Kind::DUP, empty3());
                                let n4: u32 = new_node(net, Kind::DUP, empty3());
                                relink(net, node.clone(), 1, n3, 0);
                                relink(net, node.clone(), 2, n4, 0);
                                relink(net, counter.clone(), 1, n2, 0);
                                relink(net, counter.clone(), 2, n1, 0);
                                let updated_node =
                                    &net.nodes[next as usize].as_ref().unwrap().clone();
                                set_ports(
                                    net,
                                    n3,
                                    vec![
                                        updated_node.ports[1].clone(),
                                        Port::Pointer { index: n2, slot: 1 },
                                        Port::Pointer { index: n1, slot: 1 },
                                    ],
                                );
                                set_ports(
                                    net,
                                    n4,
                                    vec![
                                        updated_node.ports[2].clone(),
                                        Port::Pointer { index: n2, slot: 2 },
                                        Port::Pointer { index: n1, slot: 2 },
                                    ],
                                );
                                let updated_counter =
                                    &net.nodes[counter_index as usize].as_ref().unwrap().clone();
                                set_ports(
                                    net,
                                    n1,
                                    vec![
                                        updated_counter.ports[2].clone(),
                                        Port::Pointer { index: n3, slot: 2 },
                                        Port::Pointer { index: n4, slot: 2 },
                                    ],
                                );
                                set_ports(
                                    net,
                                    n2,
                                    vec![
                                        updated_counter.ports[1].clone(),
                                        Port::Pointer { index: n3, slot: 1 },
                                        Port::Pointer { index: n4, slot: 1 },
                                    ],
                                );
                                remove_node(net, next);
                                remove_node(net, counter_index);
                                return true;
                            }
                            // commute 2 (c-e)
                            else if counter.kind == Kind::ERA {
                                let e1: u32 = new_node(net, Kind::ERA, vec![node.ports[1].clone()]);
                                let e2: u32 = new_node(net, Kind::ERA, vec![node.ports[2].clone()]);
                                relink(net, node.clone(), 1, e1, 0);
                                relink(net, node.clone(), 2, e2, 0);
                                remove_node(net, next);
                                remove_node(net, counter_index);
                                return true;
                            }
                            // annihilate 1 (c-c)
                            else if counter.kind == Kind::CON {
                                match node.ports[1] {
                                    Port::Pointer { index, slot: _ } if index == counter_index => {
                                        new_node(net, Kind::LOOP, vec![]);
                                    }
                                    _ => {
                                        new_node(
                                            net,
                                            Kind::WIRE,
                                            vec![node.ports[1].clone(), counter.ports[2].clone()],
                                        );
                                    }
                                }
                                match node.ports[2] {
                                    Port::Pointer { index, slot: _ } if index == counter_index => {
                                        new_node(net, Kind::LOOP, vec![]);
                                    }
                                    _ => {
                                        new_node(
                                            net,
                                            Kind::WIRE,
                                            vec![node.ports[2].clone(), counter.ports[1].clone()],
                                        );
                                    }
                                }
                                remove_node(net, next);
                                remove_node(net, counter_index);
                                return true;
                            }
                        }
                        Kind::DUP => {
                            // commute 3 (d-e)
                            if counter.kind == Kind::ERA {
                                let e1: u32 = new_node(net, Kind::ERA, vec![node.ports[1].clone()]);
                                let e2: u32 = new_node(net, Kind::ERA, vec![node.ports[2].clone()]);
                                relink(net, node.clone(), 1, e1, 0);
                                relink(net, node.clone(), 2, e2, 0);
                                remove_node(net, next);
                                remove_node(net, counter_index);
                                return true;
                            }
                            // annihilate 2 (d-d)
                            else if counter.kind == Kind::DUP {
                                match node.ports[1] {
                                    Port::Pointer { index, slot: _ } if index == counter_index => {
                                        new_node(net, Kind::LOOP, vec![]);
                                    }
                                    _ => {
                                        new_node(
                                            net,
                                            Kind::WIRE,
                                            vec![node.ports[1].clone(), counter.ports[1].clone()],
                                        );
                                    }
                                }
                                match node.ports[2] {
                                    Port::Pointer { index, slot: _ } if index == counter_index => {
                                        new_node(net, Kind::LOOP, vec![]);
                                    }
                                    _ => {
                                        new_node(
                                            net,
                                            Kind::WIRE,
                                            vec![node.ports[2].clone(), counter.ports[2].clone()],
                                        );
                                    }
                                }
                                remove_node(net, next);
                                remove_node(net, counter_index);
                                return true;
                            }
                        }
                        Kind::ERA => {
                            // annihilate 3 (e-e)
                            if counter.kind == Kind::ERA {
                                remove_node(net, next);
                                remove_node(net, counter_index);
                                return true;
                            }
                        }
                        Kind::WIRE | Kind::LOOP => {}
                    }
                }
            }
        }
        next = next + 1;
    }
    false
}

pub fn reduce_all(net: &mut Net) -> u32 {
    let mut reductions: u32 = 0;
    while reduce(net) {
        reductions += 1;
        if reductions >= 2000 {
            break;
        }
    }
    cleanup(net);
    reductions
}

pub fn trivial_commute_1() -> Net {
    Net {
        nodes: vec![
            Some(Node {
                kind: Kind::CON,
                ports: vec![
                    Port::Pointer { index: 1, slot: 0 },
                    Port::Free {
                        name: String::from("a"),
                    },
                    Port::Free {
                        name: String::from("b"),
                    },
                ],
            }),
            Some(Node {
                kind: Kind::DUP,
                ports: vec![
                    Port::Pointer { index: 0, slot: 0 },
                    Port::Free {
                        name: String::from("c"),
                    },
                    Port::Free {
                        name: String::from("d"),
                    },
                ],
            }),
        ],
    }
}

pub fn trivial_commute_2() -> Net {
    Net {
        nodes: vec![
            Some(Node {
                kind: Kind::CON,
                ports: vec![
                    Port::Pointer { index: 1, slot: 0 },
                    Port::Free {
                        name: String::from("a"),
                    },
                    Port::Free {
                        name: String::from("b"),
                    },
                ],
            }),
            Some(Node {
                kind: Kind::ERA,
                ports: vec![Port::Pointer { index: 0, slot: 0 }],
            }),
        ],
    }
}

pub fn trivial_commute_3() -> Net {
    Net {
        nodes: vec![
            Some(Node {
                kind: Kind::DUP,
                ports: vec![
                    Port::Pointer { index: 1, slot: 0 },
                    Port::Free {
                        name: String::from("a"),
                    },
                    Port::Free {
                        name: String::from("b"),
                    },
                ],
            }),
            Some(Node {
                kind: Kind::ERA,
                ports: vec![Port::Pointer { index: 0, slot: 0 }],
            }),
        ],
    }
}

pub fn trivial_annihilate_1() -> Net {
    Net {
        nodes: vec![
            Some(Node {
                kind: Kind::CON,
                ports: vec![
                    Port::Pointer { index: 1, slot: 0 },
                    Port::Free {
                        name: String::from("a"),
                    },
                    Port::Free {
                        name: String::from("b"),
                    },
                ],
            }),
            Some(Node {
                kind: Kind::CON,
                ports: vec![
                    Port::Pointer { index: 0, slot: 0 },
                    Port::Free {
                        name: String::from("c"),
                    },
                    Port::Free {
                        name: String::from("d"),
                    },
                ],
            }),
        ],
    }
}

pub fn trivial_annihilate_2() -> Net {
    Net {
        nodes: vec![
            Some(Node {
                kind: Kind::DUP,
                ports: vec![
                    Port::Pointer { index: 1, slot: 0 },
                    Port::Free {
                        name: String::from("a"),
                    },
                    Port::Free {
                        name: String::from("b"),
                    },
                ],
            }),
            Some(Node {
                kind: Kind::DUP,
                ports: vec![
                    Port::Pointer { index: 0, slot: 0 },
                    Port::Free {
                        name: String::from("c"),
                    },
                    Port::Free {
                        name: String::from("d"),
                    },
                ],
            }),
        ],
    }
}

pub fn test_annihilate_2() -> Net {
    Net {
        nodes: vec![
            Some(Node {
                kind: Kind::DUP,
                ports: vec![
                    Port::Pointer { index: 1, slot: 0 },
                    Port::Free {
                        name: String::from("a"),
                    },
                    Port::Pointer { index: 1, slot: 2 },
                ],
            }),
            Some(Node {
                kind: Kind::DUP,
                ports: vec![
                    Port::Pointer { index: 0, slot: 0 },
                    Port::Free {
                        name: String::from("c"),
                    },
                    Port::Pointer { index: 0, slot: 2 },
                ],
            }),
        ],
    }
}

pub fn trivial_annihilate_3() -> Net {
    Net {
        nodes: vec![
            Some(Node {
                kind: Kind::ERA,
                ports: vec![Port::Pointer { index: 1, slot: 0 }],
            }),
            Some(Node {
                kind: Kind::ERA,
                ports: vec![Port::Pointer { index: 0, slot: 0 }],
            }),
        ],
    }
}

pub fn nonterminating() -> Net {
    Net {
        nodes: vec![
            Some(Node {
                kind: Kind::CON,
                ports: vec![
                    Port::Pointer { index: 1, slot: 0 },
                    Port::Pointer { index: 1, slot: 2 },
                    Port::Pointer { index: 2, slot: 0 },
                ],
            }),
            Some(Node {
                kind: Kind::DUP,
                ports: vec![
                    Port::Pointer { index: 0, slot: 0 },
                    Port::Pointer { index: 3, slot: 0 },
                    Port::Pointer { index: 0, slot: 1 },
                ],
            }),
            Some(Node {
                kind: Kind::ERA,
                ports: vec![Port::Pointer { index: 0, slot: 2 }],
            }),
            Some(Node {
                kind: Kind::ERA,
                ports: vec![Port::Pointer { index: 1, slot: 1 }],
            }),
        ],
    }
}
