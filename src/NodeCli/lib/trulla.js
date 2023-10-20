import { singleton, collect, delay, toList, skip } from "./fable_modules/fable-library.4.3.0/Seq.js";
import { Solver_solve } from "./Common/CoreFiles/Solver.js";
import { renderTemplate as renderTemplate_1, renderErrors } from "./CodeGen/Renderer.js";
import { endsWith, printf, toConsole } from "./fable_modules/fable-library.4.3.0/String.js";
import * as fs from "fs";
import * as path from "path";
import { filter } from "./fable_modules/fable-library.4.3.0/List.js";
import { disposeSafe, getEnumerator } from "./fable_modules/fable-library.4.3.0/Util.js";
import { toString } from "./fable_modules/fable-library.4.3.0/Types.js";

export const proc = process;

export const args = skip(2, proc.argv);

export function renderTemplate(template) {
    let finalContent;
    const matchValue = Solver_solve(template);
    finalContent = ((matchValue.tag === 1) ? renderErrors(matchValue.fields[0]) : renderTemplate_1(matchValue.fields[0], "Trulla"));
    toConsole(printf("%s"))(finalContent);
}

export const workingDir = proc.cwd();

toConsole(`Working dir: ${workingDir}`);

export function getAllFiles(dir) {
    const files = fs.readdirSync(dir);
    return toList(delay(() => collect((file) => {
        const x = path.join(dir, "/", file);
        return fs.statSync(x).isDirectory() ? getAllFiles(x) : singleton(x);
    }, files)));
}

export const templates = filter((f) => endsWith(f, ".trulla"), getAllFiles(workingDir));

(function () {
    const enumerator = getEnumerator(templates);
    try {
        while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
            const template = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
            toConsole(`Rendering template: ${template}`);
            renderTemplate(toString(fs.readFileSync(template)));
            toConsole(`Done rendering template: ${template}`);
        }
    }
    finally {
        disposeSafe(enumerator);
    }
})();

