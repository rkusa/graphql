// #![feature(conservative_impl_trait)]

#[macro_use]
extern crate lazy_static;

// #[macro_use]
// extern crate graphql_derive;
extern crate graphql;
extern crate ctx;
extern crate core;
extern crate serde_json;
extern crate futures;

use std::collections::HashMap;
use graphql::{Resolvable, Resolve, ResolveResult, resolve, Value};

//// SCHEMA

// enum Episode { NEWHOPE, EMPIRE, JEDI }
#[derive(Clone)]
pub enum Episode {
  NewHope, // 4
  Empire, // 5
  Jedi, // 6
}

// interface Character {
//   id: String!
//   name: String
//   friends: [Character]
//   appearsIn: [Episode]
// }

pub trait Character {
  fn id(&self) -> &str;
  fn name(&self) -> Option<String>;
  fn friends(&self) -> Vec<Box<Character>>;
  fn appears_in(&self) -> Vec<Episode>;
}

#[derive(Clone)]
pub struct CharacterData {
  id: String,
  name: Option<String>,
  friends: Vec<String>,
  appears_in: Vec<Episode>,
}

// type Human implements Character {
//   id: String!
//   name: String
//   friends: [Character]
//   appearsIn: [Episode]
//   homePlanet: String
// }

#[derive(Clone)]
pub struct Human {
  data: CharacterData,
  pub home_planet: Option<String>,
}

impl Character for Human {
  fn id(&self) -> &str {
    &self.data.id
  }

  fn name(&self) -> Option<String> {
    self.data.name.clone()
  }

  fn friends(&self) -> Vec<Box<Character>> {
    get_friends(&self.data.friends)
  }

  fn appears_in(&self) -> Vec<Episode> {
    self.data.appears_in.to_vec()
  }
}

// type Droid implements Character {
//   id: String!
//   name: String
//   friends: [Character]
//   appearsIn: [Episode]
//   primaryFunction: String
// }

#[derive(Clone)]
pub struct Droid {
  data: CharacterData,
  pub primary_function: Option<String>,
}

impl Character for Droid {
  fn id(&self) -> &str {
    &self.data.id
  }

  fn name(&self) -> Option<String> {
    self.data.name.clone()
  }

  fn friends(&self) -> Vec<Box<Character>> {
    get_friends(&self.data.friends)
  }

  fn appears_in(&self) -> Vec<Episode> {
    self.data.appears_in.to_vec()
  }
}

// type Query {
//   hero(episode: Episode): Character
//   human(id: String!): Human
//   droid(id: String!): Droid
// }

pub struct Query {}

impl Query {
  pub fn hero(&self, episode: Option<i32>) -> Option<Box<Character>> {
    match episode {
        Some(5) => get_human("1000").map(|v| Box::new(v) as Box<Character>),
        _ => get_droid("2001").map(|v| Box::new(v) as Box<Character>)
    }
  }

  pub fn human(&self, id: &str) -> Option<Human> {
    get_human(id)
  }

  pub fn droid(&self, id: &str) -> Option<Droid> {
    get_droid(id)
  }
}

fn get_friends(ids: &Vec<String>) -> Vec<Box<Character>> {
  let mut result: Vec<Box<Character>> = Vec::new();
  for id in ids.iter() {
    if let Some(human) = get_human(id) {
      result.push(Box::new(human));
    } else if let Some(droid) = get_droid(id) {
      result.push(Box::new(droid));
    }
  }
  result
}

//// RESOLVABLEs

impl Into<Value> for Episode {
    fn into(self) -> Value {
        Value::Number((match self {
            Episode::NewHope => 4,
            Episode::Empire => 5,
            Episode::Jedi => 6,
        }).into())
    }
}

impl Resolvable for Box<Character> {
    fn resolve(&self, r: Resolve, name: &str) -> ResolveResult {
        match name {
            "id" => r.value(self.id()),
            "name" => r.value(self.name().unwrap()),
            "friends" => r.resolve(self.friends()),
            "appearsIn" => r.value(self.appears_in()),
            _ => r.none(),
        }
    }
}

impl Resolvable for Human {
    fn resolve(&self, r: Resolve, name: &str) -> ResolveResult {
        match name {
            "id" => r.value(self.id()),
            "name" => r.value(self.name()),
            "friends" => r.resolve(self.friends()),
            "appearsIn" => r.value(self.appears_in()),
            "homePlanet" => r.value(self.home_planet.as_ref()),
            _ => r.none(),
        }
    }
}

impl Resolvable for Droid {
    fn resolve(&self, r: Resolve, name: &str) -> ResolveResult {
        match name {
            "id" => r.value(self.id()),
            "name" => r.value(self.name()),
            "friends" => r.resolve(self.friends()),
            "appearsIn" => r.value(self.appears_in()),
            "primaryFunction" => r.value(self.primary_function.as_ref()),
            _ => r.none(),
        }
    }
}

impl Resolvable for Query {
    fn resolve(&self, r: Resolve, name: &str) -> ResolveResult {
        match name {
            "hero" => {
                let id = r.arg::<i32>("episode").ok();
                r.resolve(self.hero(id))
            }
            "human" => {
                let id = r.arg::<String>("id")?;
                r.resolve(self.human(&id))
            },
            "droid" => {
                let id = r.arg::<String>("id")?;
                r.resolve(self.droid(&id))
            },
            _ => r.none(),
        }
    }
}

fn main() {
    use ctx::background;
    use graphql::parser::parse;
    use serde_json::map::Map;
    use futures::future::Future;

    let query = "query { hero(episode: 4) { name friends { name } } }";
    let args = Map::new();
    let selection_set = parse(query, &args).unwrap();
    let root = Query{};
    let result = resolve(background(), selection_set, &root).wait();
    println!("{:?}", result);
}

//// EXAMPLE DATA

// humans
lazy_static! {
    static ref HUMANS: HashMap<&'static str, Human> = {
        let mut humans = HashMap::new();

        let luke = Human {
          data: CharacterData {
            id: "1000".to_string(),
            name: Some("Luke Skywalker".to_string()),
            friends: vec!["1002".to_string(), "1003".to_string(), "2000".to_string(), "2001".to_string()],
            appears_in: vec![Episode::NewHope, Episode::Empire, Episode::Jedi],
          },
          home_planet: Some("Tatooine".to_string()),
        };
        humans.insert("1000", luke);

        let vader = Human {
          data: CharacterData {
            id: "1001".to_string(),
            name: Some("Darth Vader".to_string()),
            friends: vec!["1004".to_string()],
            appears_in: vec![Episode::NewHope, Episode::Empire, Episode::Jedi],
          },
          home_planet: Some("Tatooine".to_string()),
        };
        humans.insert("1001", vader);

        let han = Human {
          data: CharacterData {
            id: "1002".to_string(),
            name: Some("Han Solo".to_string()),
            friends: vec!["1000".to_string(), "1003".to_string(), "2001".to_string()],
            appears_in: vec![Episode::NewHope, Episode::Empire, Episode::Jedi],
          },
          home_planet: None,
        };
        humans.insert("1002", han);

        let leia = Human {
          data: CharacterData {
            id: "1003".to_string(),
            name: Some("Leia Organa".to_string()),
            friends: vec!["1000".to_string(), "1002".to_string(), "2000".to_string(), "2001".to_string()],
            appears_in: vec![Episode::NewHope, Episode::Empire, Episode::Jedi],
          },
          home_planet: Some("Alderaan".to_string()),
        };
        humans.insert("1003", leia);

        let tarkin = Human {
          data: CharacterData {
            id: "1004".to_string(),
            name: Some("Wilhuff Tarkin".to_string()),
            friends: vec!["1001".to_string()],
            appears_in: vec![Episode::NewHope],
          },
          home_planet: None,
        };
        humans.insert("1004", tarkin);

        humans
    };
}

pub fn get_human(id: &str) -> Option<Human> {
  HUMANS.get(id).map(|v| v.to_owned())
}

// droids
lazy_static! {
    static ref DROIDS: HashMap<&'static str, Droid> = {
        let mut droids = HashMap::new();

        let threepio = Droid {
          data: CharacterData {
            id: "2000".to_string(),
            name: Some("C-3PO".to_string()),
            friends: vec!["1000".to_string(), "1002".to_string(), "1003".to_string(), "2001".to_string()],
            appears_in: vec![Episode::NewHope, Episode::Empire, Episode::Jedi],
          },
          primary_function: Some("Protocol".to_string()),
        };
        droids.insert("2000", threepio);

        let artoo = Droid {
          data: CharacterData {
            id: "2001".to_string(),
            name: Some("R2-D2".to_string()),
            friends: vec!["1000".to_string(), "1002".to_string(), "1003".to_string()],
            appears_in: vec![Episode::NewHope, Episode::Empire, Episode::Jedi],
          },
          primary_function: Some("Astromech".to_string()),
        };
        droids.insert("2001", artoo);

        droids
  };
}

pub fn get_droid(id: &str) -> Option<Droid> {
  DROIDS.get(id).map(|v| v.to_owned())
}