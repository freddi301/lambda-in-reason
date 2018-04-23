type ast('id, 'inf) =
  | Reference('id, 'inf) /* Reference(identifier, info) */
  | Application(ast('id, 'inf), ast('id, 'inf), 'inf) /* Application(left, right, info) */
  | Abstraction('id, ast('id, 'inf), 'inf); /* Abstraction(head, body, info) */