open Ast;

let rec reify = (sub, value, term) =>
  switch (term) {
  | Reference(name, _) => sub == name ? value : term
  | Abstraction(head, body, inf) =>
    sub == head ? term : Abstraction(head, reify(sub, value, body), inf)
  | Application(left, right, inf) =>
    Application(reify(sub, value, left), reify(sub, value, right), inf)
  };