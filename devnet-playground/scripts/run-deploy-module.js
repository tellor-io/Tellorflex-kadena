#!/usr/bin/env node


const { parseArgs } = require("../internal/parse-args");
const { runRequest } = require("./run-request");


const deployModule = async (detailArgs, deployArgs) => {

  const details = await parseArgs(detailArgs).then(runRequest);

  if (details.status === "failure") {
    console.log(
      "Contract not found on local Chainweb node. Deploying contract..."
    );

    const deployResult = await parseArgs(deployArgs).then(runRequest);
    if (deployResult.status === "success") {
      console.log(`Deployed! Cost: ${deployResult.gas} gas.`);
    } else {
      throw new Error(
        `Failed to deploy contract: ${JSON.stringify(
          deployResult.error,
          null,
          2
        )}`
      );
    }
  } else {
    console.log(
      `Contract exists with the name ${details.data.name} and hash ${details.data.hash}`
    );
  }
};

module.exports = { deployModule };

