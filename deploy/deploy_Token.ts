import { InMemorySigner } from "@taquito/signer";
import { MichelsonMap, TezosToolkit } from "@taquito/taquito";
import { char2Bytes } from "@taquito/utils";
import * as dotenv from "dotenv";

dotenv.config();

const SECRET_KEY = process.env.SECRET_KEY;
const ADMIN_ADDRESS = process.env.ADMIN_ADDRESS;

import Token from "../compiled/Token.mligo.json";

const RPC_ENDPOINT = "https://ghostnet.tezos.marigold.dev";

export async function DeployToken(): Promise<string | void> {
  if (!SECRET_KEY || !ADMIN_ADDRESS) return;
  const Tezos = new TezosToolkit(RPC_ENDPOINT);

  //set alice key
  Tezos.setProvider({
    signer: await InMemorySigner.fromSecretKey(SECRET_KEY),
  });

  const initialStorage = {
    extension: {
      admin: ADMIN_ADDRESS,
    },
    ledger: new Map([]),
    metadata: new Map([]),
    operators: new Map([]),
    token_metadata: new Map([]),
  };

  try {
    const originated = await Tezos.contract.originate({
      code: Token,
      storage: initialStorage,
    });
    console.log(
      `Waiting for myContract ${originated.contractAddress} to be confirmed...`
    );
    await originated.confirmation(2);
    console.log("confirmed contract: ", originated.contractAddress);
    return originated.contractAddress;
  } catch (error: any) {
    console.log(error);
  }
}
