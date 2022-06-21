#!/usr/bin/env node

import * as fs from "fs";
import assert from "node:assert/strict";

const [_nodePath, _scriptPath, ...args] = process.argv;
assert(args.length <= 1);
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
