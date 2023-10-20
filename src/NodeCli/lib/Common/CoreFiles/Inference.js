import { toString, Record, Union } from "../../fable_modules/fable-library.4.3.0/Types.js";
import { list_type, tuple_type, record_type, union_type, string_type } from "../../fable_modules/fable-library.4.3.0/Reflection.js";
import { TVar, TVar_$reflection } from "./Ast.js";
import { TrullaError, TrullaError_$reflection } from "./Core.js";
import { empty, collect, singleton, append, delay, toList } from "../../fable_modules/fable-library.4.3.0/Seq.js";
import { tryFind } from "../../fable_modules/fable-library.4.3.0/Map.js";
import { tail, head, isEmpty, empty as empty_1, singleton as singleton_1 } from "../../fable_modules/fable-library.4.3.0/List.js";
import { equals } from "../../fable_modules/fable-library.4.3.0/Util.js";
import { RangeModule_zero } from "./TheBlunt.js";
import { printf, toText } from "../../fable_modules/fable-library.4.3.0/String.js";
import { List_partitionMap3 } from "./Utils.js";
import { FSharpResult$2, FSharpChoice$3 } from "../../fable_modules/fable-library.4.3.0/Choice.js";

export class Typ extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Mono", "Poly", "Field", "Record", "Var"];
    }
}

export function Typ_$reflection() {
    return union_type("Trulla.Core.Typ", [], Typ, () => [[["Item", string_type]], [["name", string_type], ["typParam", Typ_$reflection()]], [["Item", Field_$reflection()]], [["Item", TVar_$reflection()]], [["Item", TVar_$reflection()]]]);
}

export class Field extends Record {
    constructor(name, typ) {
        super();
        this.name = name;
        this.typ = typ;
    }
}

export function Field_$reflection() {
    return record_type("Trulla.Core.Field", [], Field, () => [["name", string_type], ["typ", Typ_$reflection()]]);
}

export class Problem extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Unsolved", "Solved", "Conflicting"];
    }
}

export function Problem_$reflection() {
    return union_type("Trulla.Core.Problem", [], Problem, () => [[["Item", tuple_type(TVar_$reflection(), Typ_$reflection())]], [["Item", tuple_type(TVar_$reflection(), Typ_$reflection())]], [["Item", TrullaError_$reflection()]]]);
}

export function KnownTypes_sequenceOf(elemTypId) {
    return ["sequence", elemTypId];
}

export class Inference_Unification extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Unified", "KeepOriginal", "Conflict"];
    }
}

export function Inference_Unification_$reflection() {
    return union_type("Trulla.Core.Inference.Unification", [], Inference_Unification, () => [[["Item", list_type(Problem_$reflection())]], [], [["Item", TrullaError_$reflection()]]]);
}

export function Inference_buildProblems(tree) {
    const constrainMemberExp = (membExp) => {
        const matchValue = membExp.value;
        if (matchValue.tag === 0) {
            const accExp = matchValue.fields[0];
            return toList(delay(() => append(constrainMemberExp(accExp.instanceExp), delay(() => singleton(new Problem(0, [[accExp.instanceExp.tvar, new Typ(2, [new Field(accExp.memberName, new Typ(4, [membExp.tvar]))])]]))))));
        }
        else {
            const ident = matchValue.fields[0];
            const tvarIdent = tryFind(ident, membExp.bindingContext);
            if (tvarIdent == null) {
                return singleton_1(new Problem(0, [[new TVar(0, []), new Typ(2, [new Field(ident, new Typ(4, [membExp.tvar]))])]]));
            }
            else {
                return singleton_1(new Problem(0, [[tvarIdent, new Typ(4, [membExp.tvar])]]));
            }
        }
    };
    const constrainTree = (tree_1) => toList(delay(() => collect((tree_2) => {
        const matchValue_1 = tree_2;
        switch (matchValue_1.tag) {
            case 1: {
                const hole = matchValue_1.fields[0];
                return append(constrainMemberExp(hole), delay(() => singleton(new Problem(0, [[hole.tvar, new Typ(0, ["string"])]]))));
            }
            case 2: {
                const source = matchValue_1.fields[1];
                return append(constrainMemberExp(source), delay(() => {
                    let tupledArg;
                    return append(singleton(new Problem(0, [[source.tvar, (tupledArg = KnownTypes_sequenceOf(new Typ(4, [matchValue_1.fields[0].tvar])), new Typ(1, [tupledArg[0], tupledArg[1]]))]])), delay(() => constrainTree(matchValue_1.fields[3])));
                }));
            }
            case 3: {
                const cond = matchValue_1.fields[0];
                return append(constrainMemberExp(cond), delay(() => append(singleton(new Problem(0, [[cond.tvar, new Typ(0, ["bool"])]])), delay(() => constrainTree(matchValue_1.fields[1])))));
            }
            case 4: {
                return empty();
            }
            default: {
                return empty();
            }
        }
    }, tree_1)));
    return constrainTree(tree);
}

export function Inference_solveProblems(problems, tvarToMemberExp) {
    const subst = (tvarToReplace, withTyp, inTyp) => {
        const withTyp_1 = (withTyp.tag === 2) ? (new Typ(3, [tvarToReplace])) : withTyp;
        let matchResult, inTyp_1, name, fn, ft, tvar_1, tvarRec;
        switch (inTyp.tag) {
            case 2: {
                matchResult = 1;
                fn = inTyp.fields[0].name;
                ft = inTyp.fields[0].typ;
                break;
            }
            case 4: {
                if (equals(inTyp.fields[0], tvarToReplace)) {
                    matchResult = 2;
                    tvar_1 = inTyp.fields[0];
                }
                else {
                    matchResult = 4;
                }
                break;
            }
            case 3: {
                matchResult = 3;
                tvarRec = inTyp.fields[0];
                break;
            }
            case 0: {
                matchResult = 4;
                break;
            }
            default: {
                matchResult = 0;
                inTyp_1 = inTyp.fields[1];
                name = inTyp.fields[0];
            }
        }
        switch (matchResult) {
            case 0:
                return new Typ(1, [name, subst(tvarToReplace, withTyp_1, inTyp_1)]);
            case 1:
                return new Typ(2, [new Field(fn, subst(tvarToReplace, withTyp_1, ft))]);
            case 2:
                return withTyp_1;
            case 3: {
                let matchResult_1, tvar_3;
                if (withTyp_1.tag === 4) {
                    if (equals(tvarRec, withTyp_1.fields[0])) {
                        matchResult_1 = 0;
                        tvar_3 = withTyp_1.fields[0];
                    }
                    else {
                        matchResult_1 = 1;
                    }
                }
                else {
                    matchResult_1 = 1;
                }
                switch (matchResult_1) {
                    case 0:
                        return new Typ(3, [tvar_3]);
                    default:
                        return inTyp;
                }
            }
            default:
                return inTyp;
        }
    };
    const unify = (t1_mut, t2_mut) => {
        let arg, arg_1;
        unify:
        while (true) {
            const t1 = t1_mut, t2 = t2_mut;
            let matchResult_2, t1_2, t2_2, tv1, tv2, t, tvar_4, n1_1, n2_1, pt1_1, pt2_1, r, tvarRec_1, fn1, fn2, ft1, ft2, t1_3, t2_3;
            if (equals(t1, t2)) {
                matchResult_2 = 0;
                t1_2 = t1;
                t2_2 = t2;
            }
            else {
                switch (t1.tag) {
                    case 4: {
                        if (t2.tag === 4) {
                            matchResult_2 = 1;
                            tv1 = t1.fields[0];
                            tv2 = t2.fields[0];
                        }
                        else {
                            matchResult_2 = 2;
                            t = t2;
                            tvar_4 = t1.fields[0];
                        }
                        break;
                    }
                    case 1: {
                        switch (t2.tag) {
                            case 4: {
                                matchResult_2 = 2;
                                t = t1;
                                tvar_4 = t2.fields[0];
                                break;
                            }
                            case 1: {
                                if (t1.fields[0] === t2.fields[0]) {
                                    matchResult_2 = 3;
                                    n1_1 = t1.fields[0];
                                    n2_1 = t2.fields[0];
                                    pt1_1 = t1.fields[1];
                                    pt2_1 = t2.fields[1];
                                }
                                else {
                                    matchResult_2 = 6;
                                    t1_3 = t1;
                                    t2_3 = t2;
                                }
                                break;
                            }
                            default: {
                                matchResult_2 = 6;
                                t1_3 = t1;
                                t2_3 = t2;
                            }
                        }
                        break;
                    }
                    case 3: {
                        switch (t2.tag) {
                            case 4: {
                                matchResult_2 = 2;
                                t = t1;
                                tvar_4 = t2.fields[0];
                                break;
                            }
                            case 2: {
                                matchResult_2 = 4;
                                r = t2;
                                tvarRec_1 = t1.fields[0];
                                break;
                            }
                            default: {
                                matchResult_2 = 6;
                                t1_3 = t1;
                                t2_3 = t2;
                            }
                        }
                        break;
                    }
                    case 2: {
                        switch (t2.tag) {
                            case 4: {
                                matchResult_2 = 2;
                                t = t1;
                                tvar_4 = t2.fields[0];
                                break;
                            }
                            case 3: {
                                matchResult_2 = 4;
                                r = t1;
                                tvarRec_1 = t2.fields[0];
                                break;
                            }
                            case 2: {
                                matchResult_2 = 5;
                                fn1 = t1.fields[0].name;
                                fn2 = t2.fields[0].name;
                                ft1 = t1.fields[0].typ;
                                ft2 = t2.fields[0].typ;
                                break;
                            }
                            default: {
                                matchResult_2 = 6;
                                t1_3 = t1;
                                t2_3 = t2;
                            }
                        }
                        break;
                    }
                    default:
                        if (t2.tag === 4) {
                            matchResult_2 = 2;
                            t = t1;
                            tvar_4 = t2.fields[0];
                        }
                        else {
                            matchResult_2 = 6;
                            t1_3 = t1;
                            t2_3 = t2;
                        }
                }
            }
            switch (matchResult_2) {
                case 0:
                    return new Inference_Unification(0, [empty_1()]);
                case 1:
                    return new Inference_Unification(0, [singleton_1(new Problem(0, [[tv2, new Typ(4, [tv1])]]))]);
                case 2:
                    return new Inference_Unification(0, [singleton_1(new Problem(0, [[tvar_4, t]]))]);
                case 3: {
                    t1_mut = pt1_1;
                    t2_mut = pt2_1;
                    continue unify;
                }
                case 4:
                    return new Inference_Unification(0, [singleton_1(new Problem(0, [[tvarRec_1, r]]))]);
                case 5:
                    if (fn1 === fn2) {
                        t1_mut = ft1;
                        t2_mut = ft2;
                        continue unify;
                    }
                    else {
                        return new Inference_Unification(1, []);
                    }
                default:
                    return new Inference_Unification(2, [new TrullaError(RangeModule_zero, (arg = toString(t1_3), (arg_1 = toString(t2_3), toText(printf("Can\'t unitfy types %s and %s"))(arg)(arg_1))))]);
            }
            break;
        }
    };
    const substInProblems = (tvarToReplace_1, withType, inProblems, newProblem) => toList(delay(() => collect((matchValue_1) => {
        const ptvar = matchValue_1[0];
        const ptype_1 = subst(tvarToReplace_1, withType, matchValue_1[1]);
        if (equals(ptvar, tvarToReplace_1)) {
            const matchValue_2 = unify(withType, ptype_1);
            return (matchValue_2.tag === 1) ? singleton(newProblem([ptvar, ptype_1])) : ((matchValue_2.tag === 2) ? singleton(new Problem(2, [matchValue_2.fields[0]])) : matchValue_2.fields[0]);
        }
        else {
            return singleton(newProblem([ptvar, ptype_1]));
        }
    }, inProblems)));
    const solve = (problems_1_mut) => {
        solve:
        while (true) {
            const problems_1 = problems_1_mut;
            const patternInput = List_partitionMap3((_arg) => {
                switch (_arg.tag) {
                    case 0:
                        return new FSharpChoice$3(1, [_arg.fields[0]]);
                    case 2:
                        return new FSharpChoice$3(2, [_arg.fields[0]]);
                    default:
                        return new FSharpChoice$3(0, [_arg.fields[0]]);
                }
            }, problems_1);
            const solutions = patternInput[0];
            const problems_2 = patternInput[1];
            const conflicts = patternInput[2];
            if (isEmpty(conflicts)) {
                if (!isEmpty(problems_2)) {
                    const typ = head(problems_2)[1];
                    const tvar_5 = head(problems_2)[0];
                    problems_1_mut = toList(delay(() => append(substInProblems(tvar_5, typ, tail(problems_2), (arg_3) => (new Problem(0, [arg_3]))), delay(() => append(singleton(new Problem(1, [head(problems_2)])), delay(() => substInProblems(tvar_5, typ, solutions, (arg_4) => (new Problem(1, [arg_4])))))))));
                    continue solve;
                }
                else {
                    return new FSharpResult$2(0, [solutions]);
                }
            }
            else {
                return new FSharpResult$2(1, [conflicts]);
            }
            break;
        }
    };
    return solve(problems);
}

