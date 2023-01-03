#!/usr/bin/env node


const { deployModule } = require("./run-deploy-module.js");

const detailArgs = ["--local", "tellorflex-contract-details"];
const deployArgs = [
  "--send",
  "tellorflex-deploy",
  "--signers",
  "tellor-admin-keyset",
];

const main = async () => {
  await deployModule(detailArgs, deployArgs)
};

main();
