#!/usr/bin/env node


const { deployModule } = require("./run-deploy-module.js");

const governanceDetailArgs = ["--local", "governance-contract-details"];
const deployGovArgs = [
  "--send",
  "governance-deploy",
  "--signers",
  "tellor-admin-keyset",
];

const main = async () => {
  await deployModule(governanceDetailArgs, deployGovArgs)
};

main();
