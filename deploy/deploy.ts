import { DeployCaller } from "./deploy_Caller";
import { DeployToken } from "./deploy_Token";

async function main() {
  try {
    const token_addr = await DeployToken();
    if (typeof token_addr == "string") {
      try {
        const caller_addr = await DeployCaller(token_addr);
        if (typeof caller_addr == "string")
          console.log("Deployment successful");
        console.error("Deployment of Caller failed");
      } catch (err) {
        console.error(err);
      }
    } else {
      console.error("Deployment of Token failed");
    }
  } catch (err) {
    console.error(err);
  }
}

main();
