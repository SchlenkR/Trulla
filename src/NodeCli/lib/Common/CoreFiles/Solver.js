import { Record } from "../../fable_modules/fable-library.4.3.0/Types.js";
import { TVar, AstModule_buildTree, TExp_$reflection, TVar_$reflection } from "./Ast.js";
import { Inference_buildProblems, Inference_solveProblems, Typ_$reflection, Field_$reflection } from "./Inference.js";
import { class_type, record_type, string_type, list_type } from "../../fable_modules/fable-library.4.3.0/Reflection.js";
import { singleton, isEmpty, choose, sortBy, map as map_1, cons, filter, length, empty } from "../../fable_modules/fable-library.4.3.0/List.js";
import { Map_find } from "./Utils.js";
import { compare, safeHash, equals, comparePrimitives, int32ToString } from "../../fable_modules/fable-library.4.3.0/Util.js";
import { List_groupBy } from "../../fable_modules/fable-library.4.3.0/Seq2.js";
import { ofList } from "../../fable_modules/fable-library.4.3.0/Map.js";
import { FSharpResult$2 } from "../../fable_modules/fable-library.4.3.0/Choice.js";
import { Range$ } from "./TheBlunt.js";
import { TrullaError } from "./Core.js";
import { Parsing_parseTemplate } from "./Parsing.js";

export class RecordDef extends Record {
    constructor(id, fields, name) {
        super();
        this.id = id;
        this.fields = fields;
        this.name = name;
    }
}

export function RecordDef_$reflection() {
    return record_type("Trulla.Core.RecordDef", [], RecordDef, () => [["id", TVar_$reflection()], ["fields", list_type(Field_$reflection())], ["name", string_type]]);
}

export class Solution extends Record {
    constructor(solution, tree, records) {
        super();
        this.solution = solution;
        this.tree = tree;
        this.records = records;
    }
}

export function Solution_$reflection() {
    return record_type("Trulla.Core.Solution", [], Solution, () => [["solution", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [TVar_$reflection(), Typ_$reflection()])], ["tree", list_type(TExp_$reflection())], ["records", list_type(RecordDef_$reflection())]]);
}

export function Solver_solveParseResult(parserResult) {
    let usedRecordNames = empty();
    if (parserResult.tag === 0) {
        const matchValue = AstModule_buildTree(parserResult.fields[0].result);
        if (matchValue.tag === 0) {
            const ast = matchValue.fields[0];
            const matchValue_1 = Inference_solveProblems(Inference_buildProblems(ast.tree), ast.tvarToMemberExp);
            if (matchValue_1.tag === 0) {
                const solution = matchValue_1.fields[0];
                const makeRecord = (tvar, fields) => {
                    let lastSegmentOfMemberExp, memberExp, count;
                    return new RecordDef(tvar, fields, (tvar.tag === 1) ? ((lastSegmentOfMemberExp = ((memberExp = Map_find(new TVar(1, [tvar.fields[0]]), fields, ast.tvarToMemberExp), (memberExp.tag === 0) ? memberExp.fields[0].memberName : memberExp.fields[0])), (count = (length(filter((x) => (x === lastSegmentOfMemberExp), usedRecordNames)) | 0), (usedRecordNames = cons(lastSegmentOfMemberExp, usedRecordNames), (count === 0) ? lastSegmentOfMemberExp : (lastSegmentOfMemberExp + int32ToString(count)))))) : "Root");
                };
                let records_1;
                const _arg = map_1((tupledArg_1) => makeRecord(tupledArg_1[0], sortBy((f_1) => f_1.name, map_1((tuple_1) => tuple_1[1], tupledArg_1[1]), {
                    Compare: comparePrimitives,
                })), List_groupBy((tuple) => tuple[0], choose((tupledArg) => {
                    const t = tupledArg[1];
                    if (t.tag === 2) {
                        return [tupledArg[0], t.fields[0]];
                    }
                    else {
                        return void 0;
                    }
                }, solution), {
                    Equals: equals,
                    GetHashCode: safeHash,
                }));
                records_1 = (isEmpty(_arg) ? singleton(makeRecord(new TVar(0, []), empty())) : _arg);
                return new FSharpResult$2(0, [new Solution(ofList(solution, {
                    Compare: compare,
                }), ast.tree, records_1)]);
            }
            else {
                return new FSharpResult$2(1, [matchValue_1.fields[0]]);
            }
        }
        else {
            return new FSharpResult$2(1, [matchValue.fields[0]]);
        }
    }
    else {
        const err = parserResult.fields[0];
        return new FSharpResult$2(1, [singleton(new TrullaError(new Range$(err.idx, err.idx), err.message))]);
    }
}

export function Solver_solve(template) {
    return Solver_solveParseResult(Parsing_parseTemplate(template));
}

