#!/usr/bin/env node

/**
 * Parse the output of `cargo metadata --format-version 1` (on stdin)
 * and print all the names of all the binaries
 * produced by the current crate or workspace.
 * 
 * There's 1 optional argument.  If it's "default",
 * then use the `default-run` key in `Cargo.toml`s
 * and only print default target binaries.
 * 
 * Requires a `node` that at least supports `Array.prototype.flatMap`.
 * `node` 12 works.
 */

import * as fs from "fs";
import process from "node:process";

const [_nodePath, _scriptPath, ...args] = process.argv;
if (args.length > 1) {
    console.error("too many arguments provided")
    process.exit(1)
}
const isDefault = args[0] === "default";

const cargoMetadataString = fs.readFileSync("/dev/stdin");
const cargoMetadata = JSON.parse(cargoMetadataString);
const binaryNames = cargoMetadata
    .workspace_members
    .map(id => cargoMetadata.packages.find(e => e.id === id))
    .flatMap(package_ => {
        const useDefault = isDefault && package_.default_run;
        return package_
            .targets
            .filter(target => target.kind.includes("bin"))
            .filter(target => !useDefault ? true : target.name === package_.default_run)
            ;
    })
    .map(e => e.name)
    ;
for (const binaryName of binaryNames) {
    console.log(binaryName);
}
