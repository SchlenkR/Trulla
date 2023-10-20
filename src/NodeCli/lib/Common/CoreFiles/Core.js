import { Record } from "../../fable_modules/fable-library.4.3.0/Types.js";
import { Range$_$reflection } from "./TheBlunt.js";
import { record_type, string_type } from "../../fable_modules/fable-library.4.3.0/Reflection.js";

export class TrullaError extends Record {
    constructor(range, message) {
        super();
        this.range = range;
        this.message = message;
    }
}

export function TrullaError_$reflection() {
    return record_type("Trulla.Core.TrullaError", [], TrullaError, () => [["range", Range$_$reflection()], ["message", string_type]]);
}

