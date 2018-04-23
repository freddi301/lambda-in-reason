open Ast;

open Reify;

let rec evaluate = term =>
  switch (term) {
  | Application(Abstraction(head, body, _), right, _) =>
    evaluate(reify(head, right, body))
  | Application(left, right, inf) =>
    evaluate(Application(evaluate(left), right, inf))
  | _ => term
  };