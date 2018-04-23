open Ast;

type typeConstraint =
  | IsAbstraction(int, int);

type infereIn('id, 'inf) = {
  ast: ast('id, 'inf),
  nextType: int,
  typeScope: Belt.Map.t('id, int, string),
  constraints: Belt.Map.t(int, typeConstraint, string),
};

type infereOut('id, 'inf) = {
  ast: ast('id, 'inf),
  nextType: int,
  typ: int,
  constraints: Belt.Map.t(int, typeConstraint, string),
};

let rec infere: infereIn('id, 'inf) => infereOut('id, 'inf) =
  ({ast, nextType, typeScope, constraints}) =>
    switch (ast) {
    | Reference(name, inf) =>
      switch (Belt.Map.get(typeScope, name)) {
      | Some(typ) => {ast: Reference(name, inf /* +typ */), typ, nextType, constraints}
      | None => {typ: nextType, nextType: nextType + 1, constraints, ast: Reference(name, inf /*+nextType*/)}
      }
    | Abstraction(head, body, inf) =>
      let thisAbsType = nextType;
      let thisAbsHeadType = nextType + 1;
      let inferred =
        infere({
          ast: body,
          nextType: nextType + 2,
          typeScope: Belt.Map.set(typeScope, head, thisAbsHeadType),
          constraints,
        });
      let newConstraints =
        Belt.Map.set(inferred.constraints, thisAbsType, IsAbstraction(thisAbsHeadType, inferred.typ));
      {
        typ: thisAbsType,
        nextType: inferred.nextType,
        constraints: newConstraints,
        ast: Abstraction(head, inferred.ast, inf /*+thisAbsType*/),
      };
    | Application(Abstraction(leftHead, leftBody, leftInf), Abstraction(rightHead, rightBody, rightInf), inf) =>
      let inferred = infere({ast: Abstraction(rightHead, rightBody, rightInf), nextType, typeScope, constraints});
      let newTypeScope = Belt.Map.set(typeScope, leftHead, inferred.typ);
      infere({
        ast: leftBody,
        nextType: inferred.nextType,
        constraints: inferred.constraints,
        typeScope: newTypeScope,
      });
    | Application(left, right, inf) =>
      let inferredRigth = infere({ast: right, nextType, typeScope, constraints});
      let inferredLeft =
        infere({ast: left, nextType: inferredRigth.nextType + 1, typeScope, constraints: inferredRigth.constraints});
      let newConstraints =
        Belt.Map.set(
          inferredLeft.constraints,
          inferredLeft.typ,
          IsAbstraction(inferredRigth.typ, inferredRigth.nextType),
        );
      {
        typ: inferredRigth.nextType,
        nextType: inferredLeft.nextType,
        constraints: newConstraints,
        ast: Application(inferredLeft.ast, inferredRigth.ast, inf /*+inferredRigth.nextType*/),
      };
    };